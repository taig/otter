package io.taig.openapi.schema

import cats.Eval
import cats.syntax.all.*
import cats.data.{Chain, Validated}
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.openapi.syntax.*
import io.taig.screening.{Constraint, Validation}

sealed abstract class Dynamic[A](
    val constraints: Chain[Constraint[OpenApi]],
    val default: Option[A],
    val description: Option[String],
    val example: Option[A],
    val name: Option[String],
    tpe: String
) extends Value[A] {
  self =>

  final override type Self[a] = Dynamic[a] { type Codec = self.Codec }
  override type Codec <: OpenApi

  override def copy(
      default: Option[A],
      description: Option[String],
      example: Option[A],
      name: Option[String]
  ): Dynamic.Codec[A, Codec] = new Dynamic[A](constraints, default, description, example, name, tpe):
    export self.{decode_, encode, Codec}

  override def imap[B](f: A => B)(g: B => A): Dynamic.Codec[B, Codec] =
    new Dynamic[B](constraints, default.map(f), description, example.map(f), name, tpe):
      export self.Codec
      override def decode_(openapi: OpenApi): Validated[Violations, B] = self.decode_(openapi).map(f)
      override def encode(b: B): self.Codec = self.encode(g(b))

  override def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Dynamic.Codec[C, Codec] =
    new Dynamic[C](
      constraints ++ validation.constraints.map(_.map(_.asOpenApi)),
      default.flatMap(validation.run(_).toOption),
      description,
      example.flatMap(validation.run(_).toOption),
      name,
      tpe
    ):
      export self.Codec
      override def decode_(openapi: OpenApi): Validated[Violations, C] =
        self.decode(openapi).andThen(andThenValidate(validation, self.encode))
      override def encode(b: C): self.Codec = self.encode((g(b)))

  final override def decode(openapi: OpenApi): Validated[Violations, A] = openapi match
    case OpenApi.Null => decode_(openapi).orElse(default.toValid(nonNullViolations(tpe)))
    case openapi      => decode_(openapi)
  protected def decode_(openapi: OpenApi): Validated[Violations, A]

  override def encode(a: A): Codec
}

object Dynamic {
  type Codec[A, B <: OpenApi] = Dynamic[A] { type Codec = B }
  type Of[A <: OpenApi] = Codec[A, A]

  def apply[A <: OpenApi](tpe: String)(f: OpenApi => Option[A]): Dynamic.Of[A] =
    new Dynamic[A](Chain.empty, none, none, none, none, tpe):
      override type Codec = A
      override def decode_(openapi: OpenApi): Validated[Violations, A] =
        f(openapi).toValid(typeViolations(tpe, openapi))
      override def encode(a: A): A = a
}
