package io.taig.otter.http

import cats.Invariant
import cats.data.Validated
import cats.syntax.all.*

final case class Output[A]( /*results: Results[A], violations: Result[Violations]*/ )
//  def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Output[C] =
//    copy(results = results.ivalidate(validation)(g))
//  def imap[B](f: A => B)(g: B => A): Output[B] = copy(results = results.imap(f)(g))
//  def decode(response: Response): Validated[Violations, A] = results.decode(response) match
//    case Validated.Valid(Some(a)) => a.valid
//    case Validated.Valid(None)    =>
//      // TODO error saying that nothing matches
//      ???
//    case invalid: Validated.Invalid[?] => invalid
//  def encode(a: Validated[Violations, A]): Response = a.fold(violations.encode, results.encode)
//
//object Output:
//  abstract class Body[A]:
//    final def optional: Output.Body[Option[A]] = Output.Body.Optional(this)
//    final def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Output.Body[C] =
//      Output.Body.Validate(this, validation, g)
//    final def imap[B](f: A => B)(g: B => A): Output.Body[B] = ivalidate(Validation.lift(f))(g)
//    def decode(body: Response.Body): Validated[Violations, A]
//    def encode(a: A): Response.Body
//
//  object Body:
//    final private case class Optional[A](body: Body[A]) extends Body[Option[A]]:
//      override def decode(body: Response.Body): Validated[Violations, Option[A]] =
//        if body.isEmpty then none[A].valid else this.body.decode(body).map(_.some)
//      override def encode(a: Option[A]): Response.Body = a.fold(Response.Body.Empty)(body.encode)
//
//    final private case class Validate[A, B: Encoder, C](
//        body: Output.Body[A],
//        validation: Validation[B, A, A, C],
//        g: C => A
//    ) extends Body[C]:
//      override def decode(body: Response.Body): Validated[Violations, C] =
//        this.body.decode(body).andThen(applyValidation(validation, this.body.encode(_).asOpenApi))
//      override def encode(c: C): Response.Body = body.encode(g(c))
//
//    val Empty: Output.Body[Unit] = new Body[Unit]:
//      override def decode(body: Response.Body): Validated[Violations, Unit] = ().valid
//      override def encode(a: Unit): Response.Body = Response.Body.Empty
//
//    val Strict: Output.Body[Array[Byte]] = new Body[Array[Byte]]:
//      override def decode(body: Response.Body): Validated[Violations, Array[Byte]] = body match
//        case Response.Body.Strict(data) => data.valid
//        case _: Response.Body.Streaming =>
//          val violation = Constraint
//            .tpe(OpenApi.fromString("Response.Body.Strict"))
//            .toViolation(OpenApi.fromString("Response.Body.Streaming"))
//          Violations.rootNec(violation).invalid
//      override def encode(a: Array[Byte]): Response.Body = Response.Body.Strict(a)
//
//    val Streaming: Output.Body[Stream] = new Body[Stream]:
//      override def decode(body: Response.Body): Validated[Violations, Stream] = body match
//        case Response.Body.Strict(data)    => ??? // Stream.from(data).valid
//        case Response.Body.Streaming(data) => data.valid
//      override def encode(a: Stream): Response.Body = Response.Body.Streaming(a)
//
//    given Invariant[Output.Body] with
//      override def imap[A, B](fa: Body[A])(f: A => B)(g: B => A): Body[B] = fa.imap(f)(g)
//
//  given Invariant[Output] with
//    override def imap[A, B](fa: Output[A])(f: A => B)(g: B => A): Output[B] = fa.imap(f)(g)
