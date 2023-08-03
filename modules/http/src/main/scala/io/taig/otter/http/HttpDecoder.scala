package io.taig.otter.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.http.syntax.*
import io.taig.otter.schema.*
import io.taig.otter.validation.Constraint.Equals
import io.taig.otter.validation.{Constraint, Violation}

object HttpDecoder:
  val url: Decoder.WithRemainders[Url, (Chain[String], Chain[(String, String)])] =
    new Decoder.WithRemainders[Url, (Chain[String], Chain[(String, String)])]:
      override def decode[A](url: Url[A], data: (Chain[String], Chain[(String, String)])): Validated[Violations, A] =
        decodeWithRemainders(url, data).andThen { case ((remainders, _), a) =>
          if remainders.isEmpty then a.valid
          else
            val violation = Violation(Constraint.Equals("/"), actual = remainders.mkString_("/", "/", "").some)
            Violations.rootNec(violation).invalid
        }

      override def decodeWithRemainders[A](
          url: Url[A],
          remainders: (Chain[String], Chain[(String, String)])
      ): Validated[Violations, ((Chain[String], Chain[(String, String)]), A)] = url match {
        case Url.Empty => (remainders, ()).valid
        case Url.FromPath(path) =>
          HttpDecoder.path.decodeWithRemainders(path, remainders._1).map { case (path, a) =>
            ((path, remainders._2), a)
          }
        case Url.FromQueries(queries) =>
          HttpDecoder.queries.decodeWithRemainders(queries, remainders._2).map { case (queries, a) =>
            ((remainders._1, queries), a)
          }
        case Url.Zip(left, right)   => ???
        case Url.Modify(self, f, _) => decodeWithRemainders(self, remainders).map(_.map(f))
      }

  val path: Decoder.WithRemainders[Path, Chain[String]] = new Decoder.WithRemainders:
    override def decode[A](path: Path[A], a: Chain[String]): Validated[Violations, A] =
      decodeWithRemainders(path, a).andThen { case (remainders, a) =>
        if remainders.isEmpty then a.valid
        else
          val violation = Violation(Constraint.Equals("/"), actual = remainders.mkString_("/", "/", "").some)
          Violations.rootNec(violation).invalid
      }

    override def decodeWithRemainders[A](
        path: Path[A],
        remainders: Chain[String]
    ): Validated[Violations, (Chain[String], A)] = path match
      case Path.Empty => (remainders, ()).valid
      case Path.One(segment) =>
        HttpDecoder.segment.decodeWithRemainders(segment, remainders).leftMap(_.modifyHistory(segment.name /: _))
      case Path.Zip(left, right) =>
        decodeWithRemainders(left, remainders).andThen { case (remainders, a) =>
          decodeWithRemainders(right, remainders).map(_.tupleLeft(a))
        }
      case Path.Modify(self, f, _) => decodeWithRemainders(self, remainders).map(_.map(f))

  val segment: Decoder.WithRemainders[Segment, Chain[String]] = new Decoder.WithRemainders:
    override def decodeWithRemainders[B](
        segment: Segment[B],
        remainders: Chain[String]
    ): Validated[Violations, (Chain[String], B)] = segment match {
      case Segment.Static(name) =>
        remainders.uncons match
          case Some((head, remainders)) =>
            if head === name then (remainders, ()).valid
            else Violations.rootNec(Violation.required(head)).invalid
          case None => Violations.rootNec(Violation.required).invalid
      case Segment.Parameter(_, schema) =>
        remainders.uncons match
          case Some((head, tail)) =>
            val result = StringDecoder.value.decode(schema.value, head.some).tupleLeft(tail)
            if schema.value.isOptional
            then result.orElse(StringDecoder.value.decode(schema.value, None).tupleLeft(remainders))
            else result
          case None => Violations.rootNec(Violation.required).invalid
    }

  val queries: Decoder.WithRemainders[Queries, Chain[(String, String)]] = new Decoder.WithRemainders:
    override def decodeWithRemainders[B](
        queries: Queries[B],
        remainders: Chain[(String, String)]
    ): Validated[Violations, (Chain[(String, String)], B)] = queries match
      case Queries.Root => (remainders, ()).valid
      case Queries.One(query) =>
        HttpDecoder.query.decodeWithRemainders(query, remainders).leftMap(_.modifyHistory(query.name /: _))
      case Queries.Zip(left, right) =>
        decodeWithRemainders(left, remainders) match
          case Validated.Valid((remainders, a)) => decodeWithRemainders(right, remainders).map(_.tupleLeft(a))
          case Validated.Invalid(left) =>
            decodeWithRemainders(right, remainders) match
              case Validated.Valid(_)       => left.invalid
              case Validated.Invalid(right) => (left merge right).invalid
      case Queries.Modify(self, f, _) => decodeWithRemainders(self, remainders).map(_.map(f))

  val query: Decoder.WithRemainders[Query, Chain[(String, String)]] = new Decoder.WithRemainders:
    override def decodeWithRemainders[A](
        query: Query[A],
        remainders: Chain[(String, String)]
    ): Validated[Violations, (Chain[(String, String)], A)] = query.schema.value match
      case schema: Collection.Of[Schema.Value, ?] =>
        val (head, tail) = remainders.allWithRemainders(query.name)
        StringDecoder.collection.decode(schema, head).tupleLeft(tail)
      case schema: Schema.Value[?] =>
        remainders.firstWithRemainders(query.name) match
          case Some((value, remainders)) => StringDecoder.value.decode(schema, value.some).tupleLeft(remainders)
          case None                      => StringDecoder.value.decode(schema, None).tupleLeft(remainders)
