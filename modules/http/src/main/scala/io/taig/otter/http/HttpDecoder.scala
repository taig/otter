package io.taig.otter.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.http.syntax.*
import io.taig.otter.schema.*
import io.taig.otter.validation.Constraint.Equals
import io.taig.otter.validation.{Constraint, Violation}

object HttpDecoder:
  val request: Decoder[Request, Http.Request] = new Decoder:
    override def decode[A](request: Request[A], data: Http.Request): Validated[Violations, A] = request match
      case Request.Root(method, url, headers, body) =>
        Validated.cond(
          request.method === method,
          (),
          Violations.oneNec(
            History.Root / "method",
            Violation(Constraint.Equals(request.method.toString), actual = method.toString.some)
          )
        ) *> (
          HttpDecoder.url.decode(url, data.url).leftMap(_.modifyHistory("url" /: _)),
          HttpDecoder.headers.decode(headers, data.headers).leftMap(_.modifyHistory("headers" /: _)),
          HttpDecoder.body.decode(body, data.body).leftMap(_.modifyHistory("body" /: _))
        ).tupled
      case Request.Modify(self, f, _) => decode(self, data).map(f)

  val url: Decoder.WithRemainders[Url, Http.Url] = new Decoder.WithRemainders:
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

  val path: Decoder.WithRemainders[Path, Http.Path] = new Decoder.WithRemainders:
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

  val segment: Decoder.WithRemainders[Segment, Http.Path] = new Decoder.WithRemainders:
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
            val result = StringDecoder.value.decode(schema.value, head.some).tupleLeft(tail)
            if schema.value.isOptional
            then result.orElse(StringDecoder.value.decode(schema.value, None).tupleLeft(remainders))
            else result
          case None => Violations.rootNec(Violation.required).invalid

  val queries: Decoder.WithRemainders[Queries, Http.Queries] = new Decoder.WithRemainders:
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

  val query: Decoder.WithRemainders[Query, Http.Queries] = new Decoder.WithRemainders:
    override def decode[A](query: Query[A], data: Http.Queries): Validated[Violations, A] =
      decodeWithRemainders(query, data).map(_._2)
    override def decodeWithRemainders[A](
        query: Query[A],
        remainders: Http.Queries
    ): Validated[Violations, (Http.Queries, A)] = query.schema.value match
      case schema: Collection.Of[Schema.Value, ?] =>
        val (head, tail) = remainders.allWithRemainders(query.name)
        StringDecoder.collection.decode(schema, head).tupleLeft(tail)
      case schema: Schema.Value[?] =>
        remainders.firstWithRemainders(query.name) match
          case Some((value, remainders)) => StringDecoder.value.decode(schema, value.some).tupleLeft(remainders)
          case None                      => StringDecoder.value.decode(schema, None).tupleLeft(remainders)

  val headers: Decoder.WithRemainders[Headers, Http.Headers] = new Decoder.WithRemainders:
    override def decode[A](headers: Headers[A], data: Http.Headers): Validated[Violations, A] =
      decodeWithRemainders(headers, data).map(_._2)
    override def decodeWithRemainders[A](
        headers: Headers[A],
        remaining: Http.Headers
    ): Validated[Violations, (Http.Headers, A)] = headers match
        case Headers.Root        => (remaining, ()).valid
        case Headers.One(header) => HttpDecoder.header.decodeWithRemainders(header, remaining)

  val header: Decoder.WithRemainders[Header, Http.Headers] = new Decoder.WithRemainders:
    override def decode[A](header: Header[A], data: Http.Headers): Validated[Violations, A] =
      decodeWithRemainders(header, data).map(_._2)
    override def decodeWithRemainders[A](
        fa: Header[A],
        remaining: Http.Headers
    ): Validated[Violations, (Http.Headers, A)] = ???

  val body: Decoder[Request.Body, Http.Request.Body] = new Decoder:
    override def decode[A](body: Request.Body[A], data: Http.Request.Body): Validated[Violations, A] =
      (body, data) match
        case (Request.Body.Singlepart.Strict.Empty, _)                                         => ().valid
        case (Request.Body.Singlepart.Strict.Bytes, Http.Request.Body.Singlepart.Strict(data)) => data.valid
