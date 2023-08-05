package io.taig.otter.http

import cats.Id
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.http.Http.Request
import io.taig.otter.http.Http.Request.Body.Singlepart
import io.taig.otter.http.Input.Body.Singlepart
import io.taig.otter.http.Input.Body.Singlepart.Strict
import io.taig.otter.http.syntax.*
import io.taig.otter.schema.*
import io.taig.otter.validation.Constraint.Equals
import io.taig.otter.validation.{Constraint, Violation}

object HttpDecoder:
  val input: Decoder[Id, Input, Http.Request] = ???

  val url: Decoder.WithRemainders[Id, Url, Http.Url] = new Decoder.WithRemainders:
    override def decode[A](url: Url[A], data: Http.Url): Validated[Violations, A] =
      decodeWithRemainders(url, data).andThen { case (remainders, a) =>
        if remainders.path.isEmpty then a.valid
        else
          val violation = Violation(Constraint.Equals("/"), actual = remainders.path.mkString_("/", "/", "").some)
          Violations.rootNec(violation).invalid
      }

    override def decodeWithRemainders[A](
        url: Url[A],
        remainders: Http.Url
    ): Validated[Violations, (Http.Url, A)] = url match {
      case Url.Empty => (remainders, ()).valid
      case Url.FromPath(path) =>
        HttpDecoder.path.decodeWithRemainders(path, remainders.path).map { case (path, a) =>
          (Http.Url(path, remainders.queries), a)
        }
      case Url.FromQueries(queries) =>
        HttpDecoder.queries.decodeWithRemainders(queries, remainders.queries).map { case (queries, a) =>
          (Http.Url(remainders.path, queries), a)
        }
      case Url.Zip(left, right)   => ???
      case Url.Modify(self, f, _) => decodeWithRemainders(self, remainders).map(_.map(f))
    }

  val path: Decoder.WithRemainders[Id, Path, Http.Path] = new Decoder.WithRemainders:
    override def decode[A](path: Path[A], a: Http.Path): Validated[Violations, A] =
      decodeWithRemainders(path, a).andThen { case (remainders, a) =>
        if remainders.isEmpty then a.valid
        else
          val violation = Violation(Constraint.Equals("/"), actual = remainders.mkString_("/", "/", "").some)
          Violations.rootNec(violation).invalid
      }

    override def decodeWithRemainders[A](
        path: Path[A],
        remainders: Http.Path
    ): Validated[Violations, (Http.Path, A)] = path match
      case Path.Empty => (remainders, ()).valid
      case Path.One(segment) =>
        HttpDecoder.segment.decodeWithRemainders(segment, remainders).leftMap(_.modifyHistory(segment.name /: _))
      case Path.Zip(left, right) =>
        decodeWithRemainders(left, remainders).andThen { case (remainders, a) =>
          decodeWithRemainders(right, remainders).map(_.tupleLeft(a))
        }
      case Path.Modify(self, f, _) => decodeWithRemainders(self, remainders).map(_.map(f))

  val segment: Decoder.WithRemainders[Id, Segment, Http.Path] = new Decoder.WithRemainders:
    override def decode[A](segment: Segment[A], path: Http.Path): Validated[Violations, A] =
      decodeWithRemainders(segment, path).map(_._2)
    override def decodeWithRemainders[A](
        segment: Segment[A],
        remainders: Http.Path
    ): Validated[Violations, (Http.Path, A)] = segment match
      case Segment.Static(name) =>
        remainders.uncons match
          case Some((head, remainders)) =>
            if head === name then (remainders, ()).valid
            else Violations.rootNec(Violation.required(head)).invalid
          case None => Violations.rootNec(Violation.required).invalid
      case Segment.Parameter(_, schema) =>
        remainders.uncons match
          case Some((head, tail)) =>
            val result = StringDecoder.value.decode(schema.value, head.some).map((tail, _))
            if schema.value.isOptional
            then result.orElse(StringDecoder.value.decode(schema.value, None).map((remainders, _)))
            else result
          case None => Violations.rootNec(Violation.required).invalid

  val queries: Decoder.WithRemainders[Id, Queries, Http.Queries] = new Decoder.WithRemainders:
    override def decode[A](queries: Queries[A], data: Http.Queries): Validated[Violations, A] =
      decodeWithRemainders(queries, data).map(_._2)
    override def decodeWithRemainders[A](
        queries: Queries[A],
        remainders: Http.Queries
    ): Validated[Violations, (Http.Queries, A)] = queries match
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

  val query: Decoder.WithRemainders[Id, Query, Http.Queries] = new Decoder.WithRemainders:
    override def decode[A](query: Query[A], data: Http.Queries): Validated[Violations, A] =
      decodeWithRemainders(query, data).map(_._2)
    override def decodeWithRemainders[A](
        query: Query[A],
        remainders: Http.Queries
    ): Validated[Violations, (Http.Queries, A)] = query.schema.value match
      case schema: Collection.Of[Schema.Value, ?] =>
        val (head, tail) = remainders.allWithRemainders(query.name)
        StringDecoder.collection.decode(schema, head).map((tail, _))
      case schema: Schema.Value[?] =>
        remainders.firstWithRemainders(query.name) match
          case Some((value, remainders)) => StringDecoder.value.decode(schema, value.some).map((remainders, _))
          case None                      => StringDecoder.value.decode(schema, None).map((remainders, _))
