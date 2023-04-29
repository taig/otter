package io.taig.openapi.schema

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.openapi.validation.{Constraint, Validation}

// TODO fan out into subclasses to have a Value for primitive variants
sealed abstract class Dynamic[A] extends Schema[A]:
  self =>

  final override type Self[a] = Dynamic[a] { type Codec = self.Codec }

  final override def ivalidate[B](validation: Validation[A, A, A, B])(g: B => A): Self[B] =
    Dynamic.Validate(this, validation, g, example.flatMap(validation.run(_).toOption))

object Dynamic:
  type Codec[A, B <: OpenApi] = Dynamic[A] { type Codec = B }
  type Of[A <: OpenApi] = Codec[A, A]

  final private case class Root[A <: OpenApi](
      check: OpenApi => Option[A],
      description: Option[String],
      example: Option[A],
      tpe: String
  ) extends Dynamic[A]:
    override type Codec = A
    override def constraints: Chain[Constraint[OpenApi]] = Chain.empty
    override def modifyDescription(f: Option[String] => Option[String]): Dynamic.Of[A] =
      copy(description = f(description))
    override def modifyExample(f: Option[A] => Option[A]): Dynamic.Of[A] = copy(example = f(example))
    override def decode(openapi: OpenApi): Validated[Violations, A] =
      check(openapi).toValid(typeViolations(tpe, openapi))
    override def encode(a: A): A = a

  final private case class Validate[A, B <: OpenApi, C](
      dynamic: Dynamic.Codec[A, B],
      validation: Validation[A, A, A, C],
      g: C => A,
      example: Option[C]
  ) extends Dynamic[C]:
    override type Codec = B
    override def constraints: Chain[Constraint[OpenApi]] =
      dynamic.constraints ++ validation.constraints.map(_.map(dynamic.encode))
    override def description: Option[String] = dynamic.description
    override def modifyDescription(f: Option[String] => Option[String]): Dynamic.Codec[C, B] =
      copy(dynamic = dynamic.modifyDescription(f))
    override def modifyExample(f: Option[C] => Option[C]): Dynamic.Codec[C, B] = copy(example = f(example))
    override def decode(openapi: OpenApi): Validated[Violations, C] =
      dynamic.decode(openapi).andThen(andThenValidate(validation, dynamic.encode))
    override def encode(b: C): B = dynamic.encode(g(b))

  def apply[A <: OpenApi](tpe: String)(f: OpenApi => Option[A]): Dynamic.Of[A] = Root(f, none, none, tpe)
