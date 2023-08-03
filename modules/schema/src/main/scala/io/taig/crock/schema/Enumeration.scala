package io.taig.crock.schema

import cats.Eval
import cats.syntax.all.*
import cats.data.Chain
import io.taig.crock.validation.{Constraint, Validation}
import io.taig.enumeration.ext.Mapping

sealed abstract class Enumeration[A] extends Schema.Value[A]:
  final override type Self[a] = Enumeration[a]

  def schema: Eval[Schema.Value[?]]
  def values[B](encoder: Encoder[Schema.Value, B]): List[B]

  final override def optional: Enumeration[Option[A]] = Enumeration.Optional(this)

  override def ivalidate[B](validation: Validation[A, B])(g: B => A): Enumeration[B] =
    Enumeration.Validate(this, validation, g)

object Enumeration:
  final private[crock] case class Properties[+A](description: Option[String], example: Option[A])

  object Properties:
    val Empty: Enumeration.Properties[Nothing] = Properties(None, None)

  final private[crock] case class Root[A, B](
      mapping: Mapping[B, A],
      schema: Eval[Schema.Value[A]],
      properties: Enumeration.Properties[B]
  ) extends Enumeration[B]:
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def values[C](encoder: Encoder[Schema.Value, C]): List[C] =
      mapping.values.map(b => encoder.encode(schema.value, mapping.inj(b)))
    override def description: Property.Optional[String] = Property.Optional(
      properties.description,
      f => copy(properties = properties.copy(description = f(properties.description)))
    )
    override def example: Property.Optional[B] = Property.Optional(
      properties.example,
      f => copy(properties = properties.copy(example = f(properties.example)))
    )

  final private[crock] case class Validate[A, B](
      self: Enumeration[A],
      validation: Validation[A, B],
      g: B => A
  ) extends Enumeration[B]:
    export self.{isOptional, schema, values}
    override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
    override def description: Property.Optional[String] =
      Property.Optional(self.description.value, f => copy(self = self.description.modify(f)))
    override def example: Property.Optional[B] =
      Property.Optional(self, _.example, value => copy(self = self.example(value)), validation, g)

  final private[crock] case class Optional[A](self: Enumeration[A]) extends Enumeration[Option[A]]:
    export self.{constraints, schema, values}
    override def isOptional: Boolean = true
    override def description: Property.Optional[String] = Property.Optional(
      self.description.value,
      f => copy(self = self.description.modify(f))
    )
    override def example: Property.Optional[Option[A]] = Property.Optional(
      self.example.value.map(_.some),
      f => copy(self = self.example.modify(example => f(example.map(_.some)).flatten))
    )

  def apply[A, B](schema: Eval[Schema.Value[A]], mapping: Mapping[B, A]): Enumeration[B] =
    Root(mapping, schema, Properties.Empty)
