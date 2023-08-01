package io.taig.openapi.schema

import cats.data.Chain
import cats.syntax.all.*
import io.taig.openapi.schema
import io.taig.openapi.validation.{Constraint, Validation}

sealed abstract class Primitive[A] extends Schema.Value[A]:
  final override type Self[a] = Primitive[a]

  def tpe: Type[?]

  def format: Property.Optional[Primitive[A], String]

  final def ivalidate[B](validation: Validation[A, B])(g: B => A): Primitive[B] =
    Primitive.Validate(this, validation, g)
  final def validate(validation: Validation[A, Unit]): Primitive[A] = ivalidate(validation.tap)(identity)
  final override def imap[B](f: A => B)(g: B => A): Primitive[B] = ivalidate(Validation.lift(f))(g)

object Primitive:
  final case class Root[A](
      _description: Option[String],
      _example: Option[A],
      _format: Option[String],
      tpe: Type[A]
  ) extends Primitive[A]:
    override def constraints: Chain[Constraint] = Chain.empty
    override def description: Property.Optional[Primitive[A], String] =
      Property.Optional(_description, f => copy(_description = f(_description)))
    override def example: Property.Optional[Primitive[A], A] =
      Property.Optional(_example, f => copy(_example = f(_example)))
    override def format: Property.Optional[Primitive[A], String] =
      Property.Optional(_format, f => copy(_format = f(_format)))

  final case class Validate[A, B](primitive: Primitive[A], validation: Validation[A, B], g: B => A)
      extends Primitive[B]:
    export primitive.tpe
    override def constraints: Chain[Constraint] = primitive.constraints ++ validation.constraints
    override def description: Property.Optional[Primitive[B], String] =
      Property.Optional(primitive.description, fa => copy(primitive = fa))
    override def example: Property.Optional[Primitive[B], B] =
      Property.Optional(primitive.example, fa => copy(primitive = fa), validation, g)
    override def format: Property.Optional[Primitive[B], String] =
      Property.Optional(primitive.format, fa => copy(primitive = fa))

  def apply[A](tpe: Type[A]): Primitive[A] = Root(None, None, None, tpe)
