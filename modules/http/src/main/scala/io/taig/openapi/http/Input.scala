package io.taig.openapi.http

import cats.Invariant
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.openapi.syntax.*
import io.taig.openapi.schema.applyValidation
import io.taig.openapi.http.Input.Body
import io.taig.openapi.http.Request.Body
import io.taig.openapi.http.Request.Body.{Multipart, Singlepart}
import io.taig.openapi.schema.{Violations, Void}
import io.taig.openapi.validation.{Constraint, Validation}

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
    def decode(body: Request.Body): Validated[Violations, A]
    def encode(a: A): Request.Body

  object Body:
    abstract class Singlepart[A] extends Body[A]:
      def isStrict: Boolean
      final def optional: Input.Body.Singlepart[Option[A]] = Singlepart.Optional(this)
      final def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Input.Body.Singlepart[C] =
        Input.Body.Singlepart.Validate(this, validation, g)
      final def imap[B](f: A => B)(g: B => A): Input.Body.Singlepart[B] = ivalidate(Validation.lift(f))(g)
      final override def decode(body: Request.Body): Validated[Violations, A] = body match
        case body: Request.Body.Singlepart => decode(body)
        case _: Request.Body.Multipart =>
          val violation = Constraint
            .tpe(OpenApi.fromString("Request.Body.Singlepart"))
            .toViolation(OpenApi.fromString("Request.Body.Multipart"))
          Violations.rootNec(violation).invalid
      def decode(body: Request.Body.Singlepart): Validated[Violations, A]
      override def encode(a: A): Request.Body.Singlepart

    object Singlepart:
      final private case class Optional[A](body: Input.Body.Singlepart[A]) extends Input.Body.Singlepart[Option[A]]:
        export body.isStrict
        override def decode(body: Request.Body.Singlepart): Validated[Violations, Option[A]] =
          if body.isEmpty then none[A].valid else this.body.decode(body).map(_.some)
        override def encode(a: Option[A]): Request.Body.Singlepart = a
          .map(body.encode)
          .getOrElse:
            if isStrict
            then Request.Body.Singlepart.Strict(Array.empty)
            else Request.Body.Singlepart.Streaming(Stream.Empty)

      final private case class Validate[A, B: Encoder, C](
          body: Input.Body.Singlepart[A],
          validation: Validation[B, A, A, C],
          g: C => A
      ) extends Input.Body.Singlepart[C]:
        export body.isStrict
        override def decode(body: Request.Body.Singlepart): Validated[Violations, C] =
          this.body.decode(body).andThen(applyValidation(validation, this.body.encode(_).asOpenApi))
        override def encode(c: C): Request.Body.Singlepart = body.encode(g(c))

      val Strict: Input.Body.Singlepart[Array[Byte]] = new Singlepart[Array[Byte]]:
        override def isStrict: Boolean = true
        override def decode(body: Request.Body.Singlepart): Validated[Violations, Array[Byte]] = body match
          case Request.Body.Singlepart.Strict(data) => data.valid
          case _: Request.Body.Singlepart.Streaming =>
            val violation = Constraint
              .tpe(OpenApi.fromString("Request.Body.Singlepart.Strict"))
              .toViolation(OpenApi.fromString("Request.Body.Singlepart.Streaming"))
            Violations.rootNec(violation).invalid
        override def encode(as: Array[Byte]): Request.Body.Singlepart = Request.Body.Singlepart.Strict(as)

      val Streaming: Input.Body.Singlepart[Stream[Byte]] = new Singlepart[Stream[Byte]]:
        override def isStrict: Boolean = false
        override def decode(body: Request.Body.Singlepart): Validated[Violations, Stream[Byte]] = body match
          case _: Request.Body.Singlepart.Strict =>
            val violation = Constraint
              .tpe(OpenApi.fromString("Request.Body.Singlepart.Streaming"))
              .toViolation(OpenApi.fromString("Request.Body.Singlepart.Strict"))
            Violations.rootNec(violation).invalid
          case Request.Body.Singlepart.Streaming(data) => data.valid
        override def encode(a: Stream[Byte]): Request.Body.Singlepart = Request.Body.Singlepart.Streaming(a)

      given Invariant[Input.Body.Singlepart] with
        override def imap[A, B](fa: Input.Body.Singlepart[A])(f: A => B)(g: B => A): Input.Body.Singlepart[B] =
          fa.imap(f)(g)

    abstract class Multipart[A] extends Input.Body[A]:
      def toChain: Chain[Input.Body.Multipart.Part[?]]
      final def product[B](multipart: Input.Body.Multipart[B]): Input.Body.Multipart[(A, B)] =
        Multipart.Product(this, multipart)
      final transparent inline def zip[B](multipart: Input.Body.Multipart[B]): Input.Body.Multipart[?] =
        inline (this, multipart) match
          case (left: Multipart[Void], right) => left.product(right).imap[B] { case (_, b) => b }(b => (Void, b))
          case (left, right: Multipart[Void]) => left.product(right).imap[A] { case (a, _) => a }(a => (a, Void))
          case (left: Multipart[? *: ?], right) =>
            left.product(right).imap { case (a, b) => a :* b }(ab => (ab.init.asInstanceOf[A], ab.last.asInstanceOf[B]))
          case (left, right) => left.product(right)
      final transparent inline def :*[B](part: Input.Body.Multipart.Part[B]): Input.Body.Multipart[?] =
        zip(part.toMultipart)
      final def imap[B](f: A => B)(g: B => A): Input.Body.Multipart[B] = Multipart.Modify(this, f, g)
      override def decode(body: Request.Body): Validated[Violations, A] = body match
        case body: Request.Body.Multipart => decode(body)
        case _: Request.Body.Singlepart =>
          val violation = Constraint
            .tpe("Request.Body.Multipart")
            .toViolation(OpenApi.fromString("Request.Body.Singlepart"))
            .mapReference(OpenApi.fromString)
          Violations.rootNec(violation).invalid
      def decode(body: Request.Body.Multipart): Validated[Violations, A]
      override def encode(a: A): Request.Body.Multipart

    object Multipart:
      abstract class Part[A]:
        def name: String
        def body: Input.Body.Singlepart[?]
        final def optional: Input.Body.Multipart.Part[Option[A]] = Part.Optional(this)
        def decode(
            parts: Chain[Request.Body.Multipart.Part]
        ): Validated[Violations, (Chain[Request.Body.Multipart.Part], A)]
        def encode(a: A): Chain[Request.Body.Multipart.Part]
        final def toMultipart: Input.Body.Multipart[A] = Multipart.Root(this)

      object Part:
        final private case class Optional[A](part: Input.Body.Multipart.Part[A])
            extends Input.Body.Multipart.Part[Option[A]]:
          export part.{body, name}
          override def decode(
              parts: Chain[Request.Body.Multipart.Part]
          ): Validated[Violations, (Chain[Request.Body.Multipart.Part], Option[A])] = parts.headOption match
            // TODO
//            case Some(head) if head.name === part.name => part.decode(parts).map(_.map(_.some))
            case _ => (parts, none[A]).valid
          override def encode(a: Option[A]): Chain[Request.Body.Multipart.Part] =
            Chain.fromOption(a).flatMap(part.encode)

      final private case class Root[A](part: Input.Body.Multipart.Part[A]) extends Input.Body.Multipart[A]:
        override def toChain: Chain[Part[?]] = Chain.one(part)
        override def decode(body: Request.Body.Multipart): Validated[Violations, A] =
          part.decode(body.parts).map(_._2)
        override def encode(a: A): Request.Body.Multipart = Request.Body.Multipart(part.encode(a))

      final private case class Product[A, B](left: Input.Body.Multipart[A], right: Input.Body.Multipart[B])
          extends Input.Body.Multipart[(A, B)]:
        override def toChain: Chain[Part[?]] = left.toChain ++ right.toChain
        override def decode(body: Request.Body.Multipart): Validated[Violations, (A, B)] =
          // TODO we actually need remainders here
          (left.decode(body), right.decode(body)).tupled
        override def encode(ab: (A, B)): Request.Body.Multipart =
          Request.Body.Multipart(left.encode(ab._1).parts ++ right.encode(ab._2).parts)

      final private case class Modify[A, B](multipart: Input.Body.Multipart[A], f: A => B, g: B => A)
          extends Input.Body.Multipart[B]:
        export multipart.toChain
        override def decode(body: Request.Body.Multipart): Validated[Violations, B] =
          multipart.decode(body).map(f)
        override def encode(b: B): Request.Body.Multipart = multipart.encode(g(b))

      val Empty: Input.Body.Multipart[Void] = new Multipart[Void]:
        override def toChain: Chain[Part[?]] = Chain.empty
        override def decode(body: Request.Body.Multipart): Validated[Violations, Void] = Void.valid
        override def encode(a: Void): Request.Body.Multipart = Request.Body.Multipart.Empty

  final private case class Root[A, B, C](method: Method, url: Url[A], headers: Headers[B], body: Body[C])
      extends Input[(A, B, C)]:
    override def matches(request: Request): Boolean =
      url.matches(request.path, request.queries) && headers.matches(request.headers)
    override def decode(request: Request): Validated[Violations, (A, B, C)] = (
      // TODO adjust violation paths (?)
      // TODO check remainders (?)
      url.decode(request.path, request.queries),
      headers.decode(request.headers),
      body.decode(request.body)
    ).tupled
    override def encode(abc: (A, B, C)): Request =
      val (path, queries) = url.encode(abc._1)
      Request(method, path, queries, headers.encode(abc._2), body.encode(abc._3))

  final private case class Modify[A, B](input: Input[A], f: A => B, g: B => A) extends Input[B]:
    export input.{body, headers, matches, method, url}
    override def decode(request: Request): Validated[Violations, B] = input.decode(request).map(f)
    override def encode(b: B): Request = input.encode(g(b))

  transparent inline def apply[A, B, C](
      method: Method,
      url: Url[A],
      headers: Headers[B],
      body: Input.Body[C]
  ): Input[?] = inline (url, headers, body) match
    case _: (Url[A], Headers[Void], Input.Body[Void]) =>
      Root(method, url, headers, body).imap { case (a, _, _) => a }(a => (a, Void, Void))
    case _: (Url[Void], Headers[B], Input.Body[Void]) =>
      Root(method, url, headers, body).imap { case (_, b, _) => b }(b => (Void, b, Void))
    case _: (Url[Void], Headers[Void], Input.Body[C]) =>
      Root(method, url, headers, body).imap { case (_, _, c) => c }(c => (Void, Void, c))
    case _: (Url[A], Headers[B], Input.Body[Void]) =>
      Root(method, url, headers, body).imap { case (a, b, _) => (a, b) } { case (a, b) => (a, b, Void) }
    case _: (Url[Void], Headers[B], Input.Body[C]) =>
      Root(method, url, headers, body).imap { case (_, b, c) => (b, c) } { case (b, c) => (Void, b, c) }
    case _: (Url[A], Headers[Void], Input.Body[C]) =>
      Root(method, url, headers, body).imap { case (a, _, c) => (a, c) } { case (a, c) => (a, Void, c) }
    case _ => Root(method, url, headers, body)

  given Invariant[Input] with
    override def imap[A, B](fa: Input[A])(f: A => B)(g: B => A): Input[B] = fa.imap(f)(g)
