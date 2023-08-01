package io.taig.crock.schema

import cats.data.Chain
import io.taig.crock.validation.*
import monocle.syntax.all.*

sealed abstract class Primitive[A] extends Schema.Value[A]:
  final override type Self[a] = Primitive[a]

  def tpe: Type[?]

  def format: Property.Optional[String]

  final def ivalidate[B](validation: Validation[A, B])(g: B => A): Primitive[B] =
    Primitive.Validate(this, validation, g)
  final def validate(validation: Validation[A, Unit]): Primitive[A] = ivalidate(validation.tap)(identity)
  final override def imap[B](f: A => B)(g: B => A): Primitive[B] = ivalidate(Validation.lift(f))(g)

object Primitive:
  final case class Properties[+A](description: Option[String], example: Option[A], format: Option[String])

  object Properties:
    val Empty: Primitive.Properties[Nothing] = Properties(None, None, None)

  final case class Root[A](
      properties: Primitive.Properties[A],
      tpe: Type[A]
  ) extends Primitive[A]:
    override def constraints: Chain[Constraint] = Chain.empty
    override def description: Property.Optional[String] =
      Property.Optional(properties.description, this.focus(_.properties.description).modify)
    override def example: Property.Optional[A] =
      Property.Optional(properties.example, this.focus(_.properties.example).modify)
    override def format: Property.Optional[String] =
      Property.Optional(properties.format, this.focus(_.properties.format).modify)

  final case class Validate[A, B](schema: Primitive[A], validation: Validation[A, B], g: B => A) extends Primitive[B]:
    export schema.tpe
    override def constraints: Chain[Constraint] = schema.constraints ++ validation.constraints
    override def description: Property.Optional[String] =
      Property.Optional(schema, _.description, value => copy(schema = schema.description(value)))
    override def example: Property.Optional[B] =
      Property.Optional(schema, _.example, value => copy(schema = schema.example(value)), validation, g)
    override def format: Property.Optional[String] =
      Property.Optional(schema, _.format, value => copy(schema = schema.format(value)))

  def apply[A](tpe: Type[A]): Primitive[A] = Root(Properties.Empty, tpe)
