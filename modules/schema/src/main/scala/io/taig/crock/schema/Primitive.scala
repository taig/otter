package io.taig.crock.schema

import cats.data.Chain
import cats.syntax.all.*
import io.taig.crock.validation.*

sealed abstract class Primitive[A] extends Schema.Value[A]:
  final override type Self[a] = Primitive[a]

  def tpe: Type[?]

  def format: Property.Optional[String]

  final override def optional: Primitive[Option[A]] = Primitive.Optional(this)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Primitive[B] =
    Primitive.Validate(this, validation, g)

object Primitive:
  final case class Properties[+A](description: Option[String], example: Option[A], format: Option[String])

  object Properties:
    val Empty: Primitive.Properties[Nothing] = Properties(None, None, None)

  final case class Root[A](properties: Primitive.Properties[A], tpe: Type[A]) extends Primitive[A]:
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
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

  final case class Validate[A, B](self: Primitive[A], validation: Validation[A, B], g: B => A) extends Primitive[B]:
    export self.{isOptional, tpe}
    override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
    override def description: Property.Optional[String] =
      Property.Optional(self, _.description, value => copy(self = self.description(value)))
    override def example: Property.Optional[B] =
      Property.Optional(self, _.example, value => copy(self = self.example(value)), validation, g)
    override def format: Property.Optional[String] =
      Property.Optional(self, _.format, value => copy(self = self.format(value)))

  final case class Optional[A](self: Primitive[A]) extends Primitive[Option[A]]:
    export self.{constraints, tpe}
    override def isOptional: Boolean = true
    override def format: Property.Optional[String] = Property.Optional(
      self.format.value,
      f => copy(self = self.format.modify(f))
    )
    override def description: Property.Optional[String] = Property.Optional(
      self.description.value,
      f => copy(self = self.description.modify(f))
    )
    override def example: Property.Optional[Option[A]] = Property.Optional(
      self.example.value.map(_.some),
      f => copy(self = self.example.modify(example => f(example.map(_.some)).flatten))
    )

  def apply[A](tpe: Type[A]): Primitive[A] = Root(Properties.Empty, tpe)
