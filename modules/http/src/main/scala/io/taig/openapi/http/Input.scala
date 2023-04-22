package io.taig.openapi.http

import cats.{ApplicativeThrow, Eval, Monad}
import cats.data.{Chain, Validated}
import cats.effect.Concurrent
import cats.syntax.all.*
import io.taig.validation.identifiers
import io.taig.validation.syntax.*
import io.taig.validation.Validation
import scodec.bits.ByteVector
import fs2.Stream
import fs2.Chunk
import io.taig.openapi.OpenApi
import io.taig.openapi.schema.{andThenValidateF, Evidence, Violations}

sealed abstract class Input[A](
    val method: Method,
    val url: Url[?],
    val headers: Headers[?],
    val body: Input.Body[?]
):
  self =>

  final def imap[B](f: A => B)(g: B => A): Input[B] = ivalidate(Validation.fromFunction(f))(g)
  final def gimap[B](using evidence: Evidence.Product.Aux[B, A]): Input[B] = imap(evidence.from)(evidence.to)
  final def ivalidate[B](validation: Validation[OpenApi, A, A, B])(g: B => A): Input[B] =
    new Input[B](method, url, headers, body):
      override def decodeWithRemainders[F[+_]: Concurrent](
          request: Request[F]
      ): F[Validated[Violations, (Request[F], B)]] = self.decodeWithRemainders(request).flatMap {
        case Validated.Valid((request, a)) =>
          andThenValidateF(
            validation,
            self.encode(_).map(request => schemas.request.main.encode(request.withoutBody))
          )(a).map(_.tupleLeft(request))
        case invalid @ Validated.Invalid(_) => invalid.pure
      }

      override def encode[F[+_]: ApplicativeThrow](b: B): F[Request[F]] = self.encode(g(b))

  final def zip[B](values: Headers[B]): Input[(A, B)] = new Input[(A, B)](method, url, headers.zip(values), body):
    override def decodeWithRemainders[F[+_]: Concurrent](
        request: Request[F]
    ): F[Validated[Violations, (Request[F], (A, B))]] =
      self
        .decodeWithRemainders(request)
        .map(_.andThen { case (request, a) =>
          values
            .decodeWithRemainders(request.headers)
            .map { case (headers, b) => (request.withHeaders(headers), (a, b)) }
            .leftMap(_.modifyHistory("header" /: _))
        })

    override def encode[F[+_]: ApplicativeThrow](ab: (A, B)): F[Request[F]] =
      self.encode(ab._1).map(_.modifyHeaders(_ ++ values.encode(ab._2)))

  final def :*[B](header: Header[B]): Input[(A, B)] = zip(header.toHeaders)

  final def matches(request: Request[?]): Boolean =
    method === request.method && url.matches(request.path, request.queries)

  final def decode[F[+_]: Concurrent](request: Request[F]): F[Validated[Violations, A]] =
    decodeWithRemainders(request).map(_.map(_._2))

  def decodeWithRemainders[F[+_]: Concurrent](request: Request[F]): F[Validated[Violations, (Request[F], A)]]

  def encode[F[+_]: ApplicativeThrow](a: A): F[Request[F]]

object Input:
  sealed abstract class Body[A]:
    def imap[B](f: A => B)(g: B => A): Input.Body[B]
    def ivalidate[B](validation: Validation[OpenApi, A, A, B])(g: B => A): Input.Body[B]
    def andThen[B](f: A => Validated[Violations, B])(g: B => A): Input.Body[B]

    def decode[F[+_]: Concurrent](body: Request.Body[F]): F[Validated[Violations, A]]
    def encode[F[+_]: ApplicativeThrow](a: A): F[Request.Body[F]]

  object Body:
    abstract class Multipart[A] extends Input.Body[A]:
      self =>

      override def imap[B](f: A => B)(g: B => A): Input.Body.Multipart[B] = new Multipart[B]:
        override def decodeWithRemainder[F[+_]: Concurrent](
            body: Request.Body.Multipart[F]
        ): F[Validated[Violations, (Request.Body.Multipart[F], B)]] =
          self.decodeWithRemainder(body).map(_.map(_.map(f)))
        override def encode[F[+_]: ApplicativeThrow](b: B): F[Request.Body.Multipart[F]] = self.encode(g(b))
      override def ivalidate[B](validation: Validation[OpenApi, A, A, B])(g: B => A): Input.Body.Multipart[B] =
        new Multipart[B]:
          override def decodeWithRemainder[F[+_]: Concurrent](
              body: Request.Body.Multipart[F]
          ): F[Validated[Violations, (Request.Body.Multipart[F], B)]] =
            self.decodeWithRemainder(body).flatMap {
              case Validated.Valid((multipart, a)) =>
                andThenValidateF(
                  validation,
                  // TODO this sucks
                  self.encode(_).map(_ => schemas.request.body.encode(Request.Body.Singlepart.Empty))
                )(a).map(_.tupleLeft(multipart))
              case invalid @ Validated.Invalid(_) => invalid.pure
            }
          override def encode[F[+_]: ApplicativeThrow](b: B): F[Request.Body.Multipart[F]] = self.encode(g(b))
      override def andThen[B](f: A => Validated[Violations, B])(g: B => A): Input.Body.Multipart[B] = new Multipart[B]:
        override def decodeWithRemainder[F[+_]: Concurrent](
            body: Request.Body.Multipart[F]
        ): F[Validated[Violations, (Request.Body.Multipart[F], B)]] =
          self.decodeWithRemainder(body).map(_.andThen(_.traverse(f)))
        override def encode[F[+_]: ApplicativeThrow](b: B): F[Request.Body.Multipart[F]] = self.encode(g(b))

      def :*[B](part: Input.Body.Multipart.Part[B]): Input.Body.Multipart[(A, B)] = new Multipart[(A, B)]:
        override def decodeWithRemainder[F[+_]: Concurrent](
            body: Request.Body.Multipart[F]
        ): F[Validated[Violations, (Request.Body.Multipart[F], (A, B))]] = self.decodeWithRemainder(body).flatMap {
          case Validated.Valid((body, a)) =>
            part.decode(body.parts).map(_.map { case (parts, b) => (Request.Body.Multipart(parts), (a, b)) })
          case invalid @ Validated.Invalid(_) => invalid.pure
        }
        override def encode[F[+_]: ApplicativeThrow](ab: (A, B)): F[Request.Body.Multipart[F]] =
          (self.encode(ab._1), part.encode(ab._2)).mapN((a, b) => Request.Body.Multipart(a.parts ++ b))

      final override def decode[F[+_]: Concurrent](body: Request.Body[F]): F[Validated[Violations, A]] = body match
        case request: Request.Body.Multipart[F] => decodeWithRemainder(request).map(_.map(_._2))
        case _                                  => ???

      def decodeWithRemainder[F[+_]: Concurrent](
          body: Request.Body.Multipart[F]
      ): F[Validated[Violations, (Request.Body.Multipart[F], A)]]

      override def encode[F[+_]: ApplicativeThrow](a: A): F[Request.Body.Multipart[F]]

    object Multipart:
      abstract class Part[A]:
        self =>

        def name: String
        def body: Input.Body.Singlepart[?]

        def optional: Input.Body.Multipart.Part[Option[A]] = new Part[Option[A]] {
          override def name: String = self.name
          override def body: Input.Body.Singlepart[?] = self.body

          override def decode[F[+_]: Concurrent](
              parts: Chain[Request.Body.Multipart.Part[F]]
          ): F[Validated[Violations, (Chain[Request.Body.Multipart.Part[F]], Option[A])]] =
            parts.uncons match
              case Some((head, tail)) if head.name === name => self.decode(parts).map(_.map(_.map(_.some)))
              case _                                        => (parts, none[A]).valid.pure
          override def encode[F[+_]: ApplicativeThrow](a: Option[A]): F[Chain[Request.Body.Multipart.Part[F]]] =
            a.fold(Chain.empty[Request.Body.Multipart.Part[F]].pure[F])(self.encode)
        }

        def decode[F[+_]: Concurrent](
            parts: Chain[Request.Body.Multipart.Part[F]]
        ): F[Validated[Violations, (Chain[Request.Body.Multipart.Part[F]], A)]]
        def encode[F[+_]: ApplicativeThrow](a: A): F[Chain[Request.Body.Multipart.Part[F]]]

      object Part:
        def apply[A](name: String, body: Input.Body.Singlepart[A]): Input.Body.Multipart.Part[A] =
          val _name = name
          val _body = body

          new Part[A]:
            override def name: String = _name
            override def body: Input.Body.Singlepart[?] = _body

            override def decode[F[+_]: Concurrent](
                parts: Chain[Request.Body.Multipart.Part[F]]
            ): F[Validated[Violations, (Chain[Request.Body.Multipart.Part[F]], A)]] =
              parts.uncons match
                case Some((head, tail)) if head.name === _name => _body.decode(head.body).map(_.map(a => (tail, a)))
                case _                                         => ??? // TODO

            override def encode[F[+_]: ApplicativeThrow](a: A): F[Chain[Request.Body.Multipart.Part[F]]] =
              _body.encode(a).map(body => Chain.one(Request.Body.Multipart.Part(this.name, None, body)))

      def apply[A](part: Input.Body.Multipart.Part[A]): Input.Body.Multipart[A] = new Multipart[A]:
        override def decodeWithRemainder[F[+_]: Concurrent](
            body: Request.Body.Multipart[F]
        ): F[Validated[Violations, (Request.Body.Multipart[F], A)]] =
          part.decode(body.parts).map(_.map(_.leftMap(Request.Body.Multipart[F])))

        override def encode[F[+_]: ApplicativeThrow](a: A): F[Request.Body.Multipart[F]] =
          part.encode(a).map(Request.Body.Multipart.apply)

    abstract class Singlepart[A] extends Input.Body[A]:
      self =>

      override def imap[B](f: A => B)(g: B => A): Input.Body.Singlepart[B] = new Singlepart[B]:
        override def decode[F[+_]: Concurrent](body: Request.Body.Singlepart[F]): F[Validated[Violations, B]] =
          self.decode(body).map(_.map(f))
        override def encode[F[+_]: ApplicativeThrow](b: B): F[Request.Body.Singlepart[F]] = self.encode(g(b))
      override def ivalidate[B](validation: Validation[OpenApi, A, A, B])(g: B => A): Input.Body.Singlepart[B] =
        new Singlepart[B]:
          override def decode[F[+_]: Concurrent](body: Request.Body.Singlepart[F]): F[Validated[Violations, B]] =
            self.decode(body).flatMap {
              case Validated.Valid(a) =>
                andThenValidateF(
                  validation,
                  // TODO this sucks
                  self.encode(_).map(_ => schemas.request.body.encode(Request.Body.Singlepart.Empty))
                )(a)
              case invalid @ Validated.Invalid(_) => invalid.pure
            }
          override def encode[F[+_]: ApplicativeThrow](b: B): F[Request.Body.Singlepart[F]] = self.encode(g(b))
      override def andThen[B](f: A => Validated[Violations, B])(g: B => A): Input.Body.Singlepart[B] =
        new Singlepart[B]:
          override def decode[F[+_]: Concurrent](body: Request.Body.Singlepart[F]): F[Validated[Violations, B]] =
            self.decode(body).map(_.andThen(f))
          override def encode[F[+_]: ApplicativeThrow](b: B): F[Request.Body.Singlepart[F]] = self.encode(g(b))

      final override def decode[F[+_]: Concurrent](body: Request.Body[F]): F[Validated[Violations, A]] = body match
        case body: Request.Body.Singlepart[F] => decode(body)
        case _                                => ???
      def decode[F[+_]: Concurrent](body: Request.Body.Singlepart[F]): F[Validated[Violations, A]]

      override def encode[F[+_]: ApplicativeThrow](a: A): F[Request.Body.Singlepart[F]]

    object Singlepart:
      val streaming: Input.Body.Singlepart[Streaming[Byte]] = new Singlepart[Streaming[Byte]]:
        override def decode[F[+_]: Concurrent](
            body: Request.Body.Singlepart[F]
        ): F[Validated[Violations, Streaming[Byte]]] = Streaming.of(body.data).valid.pure[F]
        override def encode[F[+_]: ApplicativeThrow](data: Streaming[Byte]): F[Request.Body.Singlepart[F]] =
          Request.Body.Singlepart(data.toStream[F]).pure[F]

      val strict: Input.Body.Singlepart[ByteVector] = new Singlepart[ByteVector]:
        override def decode[F[+_]: Concurrent](body: Request.Body.Singlepart[F]): F[Validated[Violations, ByteVector]] =
          body.data.compile.to(ByteVector).map(_.valid)
        override def encode[F[+_]: ApplicativeThrow](a: ByteVector): F[Request.Body.Singlepart[F]] =
          Request.Body.Singlepart(Stream.chunk(Chunk.byteVector(a))).pure[F]

      val empty: Input.Body.Singlepart[Unit] = new Singlepart[Unit]:
        override def decode[F[+_]: Concurrent](body: Request.Body.Singlepart[F]): F[Validated[Violations, Unit]] =
          ().valid.pure[F]
        override def encode[F[+_]: ApplicativeThrow](a: Unit): F[Request.Body.Singlepart[F]] =
          Request.Body.Singlepart.Empty.pure[F]

  def apply[A](method: Method, url: Url[A]): Input[A] =
    val _url = url

    new Input[A](method, _url, Headers.Empty, body = Body.Singlepart.empty):
      override def decodeWithRemainders[F[+_]: Concurrent](
          request: Request[F]
      ): F[Validated[Violations, (Request[F], A)]] = {
        Validated.cond(
          this.method === request.method,
          (),
          Violations.rootNec {
            identifiers.expected
              .toConstraint(reference = OpenApi.fromString(this.method.toString).some)
              .toViolation(actual = OpenApi.fromString(request.method.toString))
          }
        ) *> _url
          .decodeWithRemainders(request.path, request.queries)
          .map { case (path, queries, a) => (request.withPath(path).withQueries(queries), a) }
      }.pure

      override def encode[F[+_]: ApplicativeThrow](a: A): F[Request[F]] =
        Body.Singlepart.empty.encode(()).map { body =>
          val (path, queries) = _url.encode(a)
          Request[F](this.method, path, queries, Chain.empty, body)
        }

  def apply[A, B](method: Method, url: Url[A], body: Body[B]): Input[(A, B)] =
    val _body = body
    val input: Input[A] = Input(method, url)

    new Input[(A, B)](method, url, Headers.Empty, _body):
      override def decodeWithRemainders[F[+_]: Concurrent](
          request: Request[F]
      ): F[Validated[Violations, (Request[F], (A, B))]] = input
        .decodeWithRemainders(request)
        .flatMap {
          case Validated.Valid((request, a)) =>
            _body.decode(request.body).map(_.map(b => (request.withoutBody, (a, b))))
          case invalid @ Validated.Invalid(_) => invalid.pure
        }

      override def encode[F[+_]: ApplicativeThrow](ab: (A, B)): F[Request[F]] =
        (input.encode(ab._1), _body.encode(ab._2)).mapN(_.withBody(_))
