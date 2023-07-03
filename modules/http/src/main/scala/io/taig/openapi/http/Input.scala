package io.taig.openapi.http

import cats.Invariant
import cats.data.Validated
import cats.syntax.all.*
import io.taig.openapi.Encoder
import io.taig.openapi.http.Request.Body
import io.taig.openapi.http.Request.Body.Singlepart
import io.taig.openapi.schema.{applyValidation, Violations}
import io.taig.openapi.syntax.*
import io.taig.openapi.validation.{Constraint, Validation, Violation}

sealed abstract class Input[A]:
  def method: Method
  def url: Url[?]
  def headers: Headers[?]
  def body: Input.Body[?]
  def matches(request: Request): Boolean
  final def imap[B](f: A => B)(g: B => A): Input[B] = Input.Modify(this, f, g)
  def decode(request: Request): Validated[Violations, A]
  def encode(a: A): Request

object Input:
  sealed abstract class Body[A]:
    self =>
    type Self[a] <: Body[a] { type Self[a] = self.Self[a] }
    def headers: Headers[?]
    def ivalidateWithHeaders[B: Encoder, C](validation: Validation[B, (Http.Headers, A), (Http.Headers, A), C])(
        g: C => (Http.Headers, A)
    ): Self[C]
    final def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Self[C] =
      ivalidateWithHeaders(
        validation
          .mapActual((Http.Headers.Empty, _))
          .contramap { case (_, a) => a }
      )(c => (Http.Headers.Empty, g(c)))
    final def imapWithHeaders[B](f: (Http.Headers, A) => B)(g: B => (Http.Headers, A)): Self[B] =
      ivalidateWithHeaders(Validation.lift(f.tupled))(g)
    final def imap[B](f: A => B)(g: B => A): Self[B] = ivalidate(Validation.lift(f))(g)
    def decodeWithRemainders(headers: Http.Headers, body: Request.Body): Validated[Violations, (Http.Headers, A)]
    def encode(a: A): (Http.Headers, Request.Body)

  object Body:
    sealed abstract class Singlepart[A] extends Input.Body[A]:
      self =>
      override type Self[a] <: Input.Body.Singlepart[a] { type Self[a] = self.Self[a] }
      def optional: Self[Option[A]]
      transparent inline def zip[B](headers: Headers[B]): Input.Body.Singlepart[?]
      override def decodeWithRemainders(
          headers: Http.Headers,
          body: Request.Body
      ): Validated[Violations, (Http.Headers, A)] = body match
        case body: Request.Body.Singlepart => decodeWithRemainders(headers, body)
      def decodeWithRemainders(
          headers: Http.Headers,
          body: Request.Body.Singlepart
      ): Validated[Violations, (Http.Headers, A)]
      override def encode(a: A): (Http.Headers, Request.Body.Singlepart)

    object Singlepart:
      sealed abstract class Strict[A] extends Input.Body.Singlepart[A]:
        final override type Self[a] = Input.Body.Singlepart.Strict[a]
        override def optional: Input.Body.Singlepart.Strict[Option[A]] = Strict.Optional(this)

        final def product[B](headers: Headers[B]): Input.Body.Singlepart.Strict[(A, B)] = Strict.Product(this, headers)
        final transparent inline def zip[B](headers: Headers[B]): Input.Body.Singlepart.Strict[?] =
          inline (this, headers) match
            case (left: Body[Unit], right)    => left.product(right).imap[B] { case (_, b) => b }(b => ((), b))
            case (left, right: Headers[Unit]) => left.product(right).imap[A] { case (a, _) => a }(a => (a, ()))
            case (left: Body[? *: ?], right) =>
              left
                .product(right)
                .imap { case (a, b) => a :* b }(ab => (ab.init.asInstanceOf[A], ab.last.asInstanceOf[B]))
            case (left, right) => left.product(right)
        final transparent inline def :*[B](header: Header[B]): Input.Body.Singlepart.Strict[?] = zip(header.toHeaders)

        override def ivalidateWithHeaders[B: Encoder, C](
            validation: Validation[B, (Http.Headers, A), (Http.Headers, A), C]
        )(g: C => (Http.Headers, A)): Strict[C] = Strict.ValidateWithHeaders(this, validation, g)
        override def decodeWithRemainders(
            headers: Http.Headers,
            body: Request.Body.Singlepart
        ): Validated[Violations, (Http.Headers, A)] = body match
          case body: Request.Body.Singlepart.Strict => decodeWithRemainders(headers, body)
          case _: Request.Body.Singlepart.Streaming =>
            val violation = Violation(
              Constraint.tpe("Request.Body.Singlepart.Strict".asOpenApi),
              "Request.Body.Singlepart.Streaming".asOpenApi
            )
            Violations.rootNec(violation).invalid
        def decodeWithRemainders(
            headers: Http.Headers,
            body: Request.Body.Singlepart.Strict
        ): Validated[Violations, (Http.Headers, A)]
        override def encode(a: A): (Http.Headers, Request.Body.Singlepart.Strict)

      object Strict:
        final private case class Optional[A](body: Input.Body.Singlepart.Strict[A])
            extends Input.Body.Singlepart.Strict[Option[A]]:
          export body.headers
          override def decodeWithRemainders(
              headers: Http.Headers,
              body: Request.Body.Singlepart.Strict
          ): Validated[Violations, (Http.Headers, Option[A])] =
            if body.bytes.isEmpty then (headers, none[A]).valid
            else this.body.decodeWithRemainders(headers, body).map(_.map(_.some))
          override def encode(a: Option[A]): (Http.Headers, Request.Body.Singlepart.Strict) =
            a.fold((Http.Headers.Empty, Request.Body.Singlepart.Strict.Empty))(body.encode)

        final private case class Product[A, B](body: Input.Body.Singlepart.Strict[A], headers: Headers[B])
            extends Input.Body.Singlepart.Strict[(A, B)]:
          override def decodeWithRemainders(
              headers: Http.Headers,
              body: Request.Body.Singlepart.Strict
          ): Validated[Violations, (Http.Headers, (A, B))] =
            this.body
              .decodeWithRemainders(headers, body)
              .andThen { case (headers, a) => this.headers.decodeWithRemainders(headers).map(_.tupleLeft(a)) }
          override def encode(ab: (A, B)): (Http.Headers, Request.Body.Singlepart.Strict) =
            body.encode(ab._1).leftMap(_ merge headers.encode(ab._2))

        final private case class ValidateWithHeaders[A, B: Encoder, C](
            body: Input.Body.Singlepart.Strict[A],
            validation: Validation[B, (Http.Headers, A), (Http.Headers, A), C],
            g: C => (Http.Headers, A)
        ) extends Input.Body.Singlepart.Strict[C]:
          export body.headers
          override def decodeWithRemainders(
              headers: Http.Headers,
              body: Request.Body.Singlepart.Strict
          ): Validated[Violations, (Http.Headers, C)] = this.body
            .decodeWithRemainders(headers, body)
            .andThen(
              _.traverse(
                applyValidation(validation.mapActual(_._2).contramap((headers, _)), this.body.encode(_)._2.asOpenApi)
              )
            )
          override def encode(c: C): (Http.Headers, Request.Body.Singlepart.Strict) =
            val (headers, a) = g(c)
            body.encode(a).leftMap(_ merge headers)

        val Bytes: Input.Body.Singlepart.Strict[Array[Byte]] = new Strict[Array[Byte]]:
          override def headers: Headers[?] = Headers.Empty
          override def decodeWithRemainders(
              headers: Http.Headers,
              body: Request.Body.Singlepart.Strict
          ): Validated[Violations, (Http.Headers, Array[Byte])] = (headers, body.bytes).valid
          override def encode(a: Array[Byte]): (Http.Headers, Request.Body.Singlepart.Strict) =
            (Http.Headers.Empty, Request.Body.Singlepart.Strict(a))

        val Empty: Input.Body.Singlepart.Strict[Unit] = new Strict[Unit]:
          override def headers: Headers[?] = Headers.Empty
          override def decodeWithRemainders(
              headers: Http.Headers,
              body: Request.Body.Singlepart.Strict
          ): Validated[Violations, (Http.Headers, Unit)] = (Http.Headers.Empty, ()).valid
          override def encode(a: Unit): (Http.Headers, Request.Body.Singlepart.Strict) =
            (Http.Headers.Empty, Request.Body.Singlepart.Strict.Empty)

      sealed abstract class Streaming[A] extends Input.Body.Singlepart[A] {
        final override type Self[a] = Input.Body.Singlepart.Streaming[a]
        final override def optional: Streaming[Option[A]] = ???
        final override def ivalidateWithHeaders[B: Encoder, C](
            validation: Validation[B, (Http.Headers, A), (Http.Headers, A), C]
        )(g: C => (Http.Headers, A)): Streaming[C] = ???
        final override def decodeWithRemainders(
            headers: Http.Headers,
            body: Request.Body.Singlepart
        ): Validated[Violations, (Http.Headers, A)] = ???
        def decode(headers: Http.Headers, body: Request.Body.Singlepart.Streaming): Validated[Violations, A]
        override def encode(a: A): (Http.Headers, Request.Body.Singlepart.Streaming)
      }

//      final private case class Strict[A](headers: Headers[A]) extends Input.Body.Singlepart[(A, Array[Byte])]:
//        override def decode(
//            headers: Http.Headers,
//            body: Request.Body.Singlepart
//        ): F[Validated[Violations, (A, Array[Byte])]] =
//          body.entity.consume.map(this.headers.decode(headers).tupleRight)
//        override def encode(
//            ab: (A, Array[Byte])
//        ): F[(Http.Headers, Request.Body.Singlepart)] =
//          (headers.encode(ab._1), Request.Body.Singlepart(Entity.strict(ab._2))).pure[F]
//
//      final private case class Streaming[A](headers: Headers[A]) extends Input.Body.Singlepart[(A, Entity)]:
//        override def decode(
//            headers: Http.Headers,
//            body: Request.Body.Singlepart
//        ): F[Validated[Violations, (A, Entity)]] = this.headers.decode(headers).tupleRight(body.entity).pure[F]
//        override def encode[F[+_]](ab: (A, Entity))(using
//            F: ApplicativeThrow[F]
//        ): F[(Http.Headers, Request.Body.Singlepart)] =
//          (headers.encode(ab._1), Request.Body.Singlepart(ab._2)).pure[F]
//
//      final case class Optional[A](body: Input.Body.Singlepart[A]) extends Input.Body.Singlepart[Option[A]]:
//        export body.headers
//        override def encode(a: Option[A]): F[(Http.Headers, Request.Body.Singlepart)] =
//          a.fold((Http.Headers.Empty, Request.Body.Singlepart(Entity.Empty)).pure[F])(body.encode(_))
//        override def decode(
//            headers: Http.Headers,
//            body: Request.Body.Singlepart
//        ): F[Validated[Violations, Option[A]]] =
//          if body.entity.isEmpty
//          then none[A].valid.pure[F]
//          else this.body.decode(headers, body).map(_.map(_.some))
//
//      final case class ValidateWithHeaders[A, B: Encoder, C](
//          body: Input.Body.Singlepart[A],
//          validation: Validation[B, (Http.Headers, A), (Http.Headers, A), C],
//          g: C => (Http.Headers, A)
//      ) extends Input.Body.Singlepart[C]:
//        export body.headers
//        override def decode(
//            headers: Http.Headers,
//            body: Request.Body.Singlepart
//        ): F[Validated[Violations, C]] = this.body
//          .decode(headers, body)
//          .map:
//            _.andThen(a =>
//              applyValidation(validation, _ => OpenApi.fromString("Input.Body.Singlepart(...)"))((headers, a))
//            )
//        override def encode(c: C): F[(Http.Headers, Request.Body.Singlepart)] =
//          body.encode(g(c)._2)
//
//      val Empty: Input.Body.Singlepart[Unit] = new Singlepart[Unit]:
//        override def headers: Headers[?] = Headers.Empty
//        override def decode(
//            headers: Http.Headers,
//            body: Request.Body.Singlepart
//        ): F[Validated[Violations, Unit]] = ().valid.pure[F]
//        override def encode(a: Unit): F[(Http.Headers, Request.Body.Singlepart)] =
//          (Http.Headers.Empty, Request.Body.Singlepart(Entity.Empty)).pure[F]
//
//      transparent inline def strict[A](headers: Headers[A]): Input.Body.Singlepart[?] = inline headers match
//        case _: Headers[Unit] =>
//          Strict(Headers.Empty)
//            .imap { case (_, bytes) => bytes }(bytes => ((), bytes)): Input.Body.Singlepart[Array[Byte]]
//        case _ => Strict(headers): Input.Body.Singlepart[(A, Array[Byte])]
//
//      transparent inline def streaming[A](headers: Headers[A]): Input.Body.Singlepart[?] = inline headers match
//        case _: Headers[Unit] => Strict(headers).imap { case (_, bytes) => bytes }(bytes => ((), bytes))
//        case _                => Strict(headers)

//    abstract class Multipart[A] extends Input.Body[A]:
//      def toChain: Chain[Input.Body.Multipart.Part[?]]
//      final def product[B](multipart: Input.Body.Multipart[B]): Input.Body.Multipart[(A, B)] =
//        Multipart.Product(this, multipart)
//      final transparent inline def zip[B](multipart: Input.Body.Multipart[B]): Input.Body.Multipart[?] =
//        inline (this, multipart) match
//          case (left: Multipart[Void], right) => left.product(right).imap[B] { case (_, b) => b }(b => (Void, b))
//          case (left, right: Multipart[Void]) => left.product(right).imap[A] { case (a, _) => a }(a => (a, Void))
//          case (left: Multipart[? *: ?], right) =>
//            left.product(right).imap { case (a, b) => a :* b }(ab => (ab.init.asInstanceOf[A], ab.last.asInstanceOf[B]))
//          case (left, right) => left.product(right)
//      final transparent inline def :*[B](part: Input.Body.Multipart.Part[B]): Input.Body.Multipart[?] =
//        zip(part.toMultipart)
//      final def imap[B](f: A => B)(g: B => A): Input.Body.Multipart[B] = Multipart.Modify(this, f, g)
////      override def decode(body: Request.Body): Validated[Violations, A] = body match
////        case body: Request.Body.Multipart => decode(body)
////        case _: Request.Body.Singlepart =>
////          val violation = Constraint
////            .tpe("Request.Body.Multipart")
////            .toViolation(OpenApi.fromString("Request.Body.Singlepart"))
////            .mapReference(OpenApi.fromString)
////          Violations.rootNec(violation).invalid
////      def decode(body: Request.Body.Multipart): Validated[Violations, A]
//      override def decode[F[_]](body: Request.Body): F[Validated[Violations, A]] = ???
//      override def encode(a: A): Request.Body.Multipart
//
//    object Multipart:
//      abstract class Part[A]:
//        def name: String
//        def body: Input.Body.Singlepart[?]
//        final def optional: Input.Body.Multipart.Part[Option[A]] = Part.Optional(this)
//        def decode(
//            parts: Chain[Request.Body.Multipart.Part]
//        ): Validated[Violations, (Chain[Request.Body.Multipart.Part], A)]
//        def encode(a: A): Chain[Request.Body.Multipart.Part]
//        final def toMultipart: Input.Body.Multipart[A] = Multipart.Root(this)
//
//      object Part:
//        final private case class Optional[A](part: Input.Body.Multipart.Part[A])
//            extends Input.Body.Multipart.Part[Option[A]]:
//          export part.{body, name}
//          override def decode(
//              parts: Chain[Request.Body.Multipart.Part]
//          ): Validated[Violations, (Chain[Request.Body.Multipart.Part], Option[A])] = parts.headOption match
//            // TODO
////            case Some(head) if head.name === part.name => part.decode(parts).map(_.map(_.some))
//            case _ => (parts, none[A]).valid
//          override def encode(a: Option[A]): Chain[Request.Body.Multipart.Part] =
//            Chain.fromOption(a).flatMap(part.encode)
//
//      final private case class Root[A](part: Input.Body.Multipart.Part[A]) extends Input.Body.Multipart[A]:
//        override def toChain: Chain[Part[?]] = Chain.one(part)
////        override def decode(body: Request.Body.Multipart): Validated[Violations, A] =
////          part.decode(body.parts).map(_._2)
//        override def encode(a: A): Request.Body.Multipart = Request.Body.Multipart(part.encode(a))
//
//      final private case class Product[A, B](left: Input.Body.Multipart[A], right: Input.Body.Multipart[B])
//          extends Input.Body.Multipart[(A, B)]:
//        override def toChain: Chain[Part[?]] = left.toChain ++ right.toChain
////        override def decode(body: Request.Body.Multipart): Validated[Violations, (A, B)] =
////           TODO we actually need remainders here
////          (left.decode(body), right.decode(body)).tupled
//        override def encode(ab: (A, B)): Request.Body.Multipart =
//          Request.Body.Multipart(left.encode(ab._1).parts ++ right.encode(ab._2).parts)
//
//      final private case class Modify[A, B](multipart: Input.Body.Multipart[A], f: A => B, g: B => A)
//          extends Input.Body.Multipart[B]:
//        export multipart.toChain
////        override def decode(body: Request.Body.Multipart): Validated[Violations, B] =
////          multipart.decode(body).map(f)
//        override def encode(b: B): Request.Body.Multipart = multipart.encode(g(b))
//
//      val Empty: Input.Body.Multipart[Void] = new Multipart[Void]:
//        override def toChain: Chain[Part[?]] = Chain.empty
////        override def decode(body: Request.Body.Multipart): Validated[Violations, Void] = Void.valid
//        override def encode(a: Void): Request.Body.Multipart = Request.Body.Multipart.Empty

  final private case class Root[A, B, C](method: Method, url: Url[A], headers: Headers[B], body: Body[C])
      extends Input[(A, B, C)]:
    override def matches(request: Request): Boolean =
      url.matches(request.path, request.queries) && headers.matches(request.headers)
    override def encode(abc: (A, B, C)): Request =
      val (headers, body) = this.body.encode(abc._3)
      val (path, queries) = url.encode(abc._1)
      Request(method, path, queries, this.headers.encode(abc._2) merge headers, body)
    override def decode(request: Request): Validated[Violations, (A, B, C)] = ???

  final private case class Modify[A, B](input: Input[A], f: A => B, g: B => A) extends Input[B]:
    export input.{body, headers, matches, method, url}
    override def decode(request: Request): Validated[Violations, B] =
      input.decode(request).map(f)
    override def encode(b: B): Request = input.encode(g(b))

  transparent inline def apply[A, B, C](
      method: Method,
      url: Url[A],
      headers: Headers[B],
      body: Input.Body[C]
  ): Input[?] = inline (url, headers, body) match
    case _: (Url[A], Headers[Unit], Input.Body[Unit]) =>
      Root(method, url, headers, body).imap { case (a, _, _) => a }(a => (a, (), ()))
    case _: (Url[Unit], Headers[B], Input.Body[Unit]) =>
      Root(method, url, headers, body).imap { case (_, b, _) => b }(b => ((), b, ()))
    case _: (Url[Unit], Headers[Unit], Input.Body[C]) =>
      Root(method, url, headers, body).imap { case (_, _, c) => c }(c => ((), (), c))
    case _: (Url[A], Headers[B], Input.Body[Unit]) =>
      Root(method, url, headers, body).imap { case (a, b, _) => (a, b) } { case (a, b) => (a, b, ()) }
    case _: (Url[Unit], Headers[B], Input.Body[C]) =>
      Root(method, url, headers, body).imap { case (_, b, c) => (b, c) } { case (b, c) => ((), b, c) }
    case _: (Url[A], Headers[Unit], Input.Body[C]) =>
      Root(method, url, headers, body).imap { case (a, _, c) => (a, c) } { case (a, c) => (a, (), c) }
    case _ => Root(method, url, headers, body)

  given Invariant[Input] with
    override def imap[A, B](fa: Input[A])(f: A => B)(g: B => A): Input[B] = fa.imap(f)(g)
