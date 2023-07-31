package io.taig.openapi.schema

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.validation.{Constraint, Validation, Violation}
import io.taig.openapi.{schema, OpenApi}

sealed abstract class Primitive[A] extends Schema.Value[A]:
  final override type Self[a] = Primitive[a]

  def tpe: Type[?]

  final def ivalidate[B](validation: Validation[A, B])(g: B => A): Primitive[B] =
    Primitive.Validate(this, validation, g)
  final override def imap[B](f: A => B)(g: B => A): Primitive[B] = ivalidate(Validation.lift(f))(g)

object Primitive:
  final case class Root[A](
      description: Property.Optional[Primitive[A], String],
      _example: Option[A],
      _format: Option[String],
      tpe: Type[A]
  ) extends Primitive[A]:
    override def constraints: Chain[Constraint] = Chain.empty

  final case class Validate[A, B](primitive: Primitive[A], validation: Validation[A, B], g: B => A)
      extends Primitive[B]:
    export primitive.tpe
    override def constraints: Chain[Constraint] = primitive.constraints ++ validation.constraints

  def apply[A](tpe: Type[A]): Primitive[A] = ??? // Root(Property.Optional.apply(???, ???), None, None, tpe)
