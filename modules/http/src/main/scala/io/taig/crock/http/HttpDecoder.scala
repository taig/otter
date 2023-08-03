package io.taig.crock.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.crock.http.syntax.*
import io.taig.crock.schema.*
import io.taig.crock.validation.Constraint.Equals
import io.taig.crock.validation.{Constraint, Violation}

final class HttpDecoder(plain: Decoder[Schema.Value, Option[String]]):
  self =>

  val url: Decoder[Url, (Chain[String], Chain[(String, String)])] = new Decoder:
    override def decode[a](url: Url[a], a: (Chain[String], Chain[(String, String)])): Validated[Violations, a] =
      (
        self.path.decode(url.path, a._1).leftMap(_.modifyHistory("path" /: _)),
        self.queries.decode(url.queries, a._2).leftMap(_.modifyHistory("queries" /: _))
      )
      ???

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
        self.segment.decodeWithRemainders(segment, remainders).leftMap(_.modifyHistory(segment.name /: _))
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
      case Segment.Parameter(name, schema) =>
        remainders.uncons match
          case Some((head, tail)) =>
            val result = plain.decode(schema.value, head.some).tupleLeft(tail)
            if schema.value.isOptional
            then result.orElse(plain.decode(schema.value, None).tupleLeft(remainders))
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
        self.query.decodeWithRemainders(query, remainders).leftMap(_.modifyHistory(query.name /: _))
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

object HttpDecoder:
  def default: HttpDecoder = new HttpDecoder(StringDecoder.value)
