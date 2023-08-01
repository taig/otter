package io.taig.crock.schema

import cats.data.Chain
import io.taig.crock.validation.*

sealed abstract class Primitive[A] extends Schema.Value[A]:
  final override type Self[a] = Primitive[a]

  def tpe: Type[?]

  def format: Property.Optional[String]

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Primitive[B] =
    Primitive.Validate(this, validation, g)

object Primitive:
  final case class Properties[+A](description: Option[String], example: Option[A], format: Option[String])

  object Properties:
    val Empty: Primitive.Properties[Nothing] = Properties(None, None, None)

  final case class Root[A](
      properties: Primitive.Properties[A],
      tpe: Type[A]
  ) extends Primitive[A]:
    override def constraints: Chain[Constraint] = Chain.empty
    override def description: Property.Optional[String] = Property.Optional(
      properties.description,
      f => copy(properties = properties.copy(description = f(properties.description)))
    )
    override def example: Property.Optional[A] = Property.Optional(
      properties.example,
      f => copy(properties = properties.copy(example = f(properties.example)))
    )
    override def format: Property.Optional[String] = Property.Optional(
      properties.format,
      f => copy(properties = properties.copy(format = f(properties.format)))
    )

  final case class Validate[A, B](primitive: Primitive[A], validation: Validation[A, B], g: B => A)
      extends Primitive[B]:
    export primitive.tpe
    override def constraints: Chain[Constraint] = primitive.constraints ++ validation.constraints
    override def description: Property.Optional[String] =
      Property.Optional(primitive, _.description, value => copy(primitive = primitive.description(value)))
    override def example: Property.Optional[B] =
      Property.Optional(primitive, _.example, value => copy(primitive = primitive.example(value)), validation, g)
    override def format: Property.Optional[String] =
      Property.Optional(primitive, _.format, value => copy(primitive = primitive.format(value)))

  def apply[A](tpe: Type[A]): Primitive[A] = Root(Properties.Empty, tpe)
