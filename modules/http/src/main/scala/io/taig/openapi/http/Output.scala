package io.taig.openapi.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.openapi.syntax.*
import io.taig.openapi.http.Response.Body
import io.taig.openapi.schema.applyValidation
import io.taig.openapi.schema.{Violations, Void}
import io.taig.openapi.validation.{Constraint, Validation}

sealed abstract class Output[A]:
  def imap[B](f: A => B)(g: B => A): Output[B] = ???

object Output:
  abstract class Body[A]:
    final def optional: Output.Body[Option[A]] = Output.Body.Optional(this)
    final def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Output.Body[C] =
      Output.Body.Validate(this, validation, g)
    final def imap[B](f: A => B)(g: B => A): Output.Body[B] = ivalidate(Validation.lift(f))(g)
    def decode(body: Response.Body): Validated[Violations, A]
    def encode(a: A): Response.Body

  object Body:
    final private case class Optional[A](body: Body[A]) extends Body[Option[A]]:
      override def decode(body: Response.Body): Validated[Violations, Option[A]] = body match
        case Response.Body.Strict(data)    => if data.isEmpty then none[A].valid else this.body.decode(body).map(_.some)
        case Response.Body.Streaming(data) => if data.isEmpty then none[A].valid else this.body.decode(body).map(_.some)
      override def encode(a: Option[A]): Response.Body = a.fold(Response.Body.Empty)(body.encode)

    final private case class Validate[A, B: Encoder, C](
        body: Output.Body[A],
        validation: Validation[B, A, A, C],
        g: C => A
    ) extends Body[C]:
      override def decode(body: Response.Body): Validated[Violations, C] =
        this.body.decode(body).andThen(applyValidation(validation, this.body.encode(_).asOpenApi))
      override def encode(c: C): Response.Body = body.encode(g(c))

    val Empty: Output.Body[Void] = new Body[Void]:
      override def decode(body: Response.Body): Validated[Violations, Void] = Void.valid
      override def encode(a: Void): Response.Body = Response.Body.Empty

    val Strict: Output.Body[Array[Byte]] = new Body[Array[Byte]]:
      override def decode(body: Response.Body): Validated[Violations, Array[Byte]] = body match
        case Response.Body.Strict(data) => data.valid
        case _: Response.Body.Streaming =>
          val violation = Constraint
            .tpe(OpenApi.fromString("Response.Body.Strict"))
            .toViolation(OpenApi.fromString("Response.Body.Streaming"))
          Violations.rootNec(violation).invalid
      override def encode(a: Array[Byte]): Response.Body = Response.Body.Strict(a)

    val Streaming: Output.Body[Stream[Byte]] = new Body[Stream[Byte]]:
      override def decode(body: Response.Body): Validated[Violations, Stream[Byte]] = body match
        case Response.Body.Strict(data)    => Stream.from(data).valid
        case Response.Body.Streaming(data) => data.valid
      override def encode(a: Stream[Byte]): Response.Body = Response.Body.Streaming(a)
