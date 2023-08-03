package io.taig.crock.schema

import cats.Eval
import cats.syntax.all.*
import cats.data.Chain
import io.taig.crock.validation.{Constraint, Validation}

sealed abstract class Product[A] extends Schema[A]:
  override type Self[a] = Product[a]
  def toChain: Chain[Eval[Schema[?]]]
  final override def optional: Product[Option[A]] = Product.Optional(this)
  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Product[B] =
    Product.Validate(this, validation, g)
  final def to[B](using evidence: Evidence.Product.Aux[B, A]): Product[B] = imap(evidence.from)(evidence.to)

object Product:
  final private[crock] case class Properties[+A](description: Option[String], example: Option[A])

  object Properties:
    val Empty: Product.Properties[Nothing] = Properties(None, None)

  final private[crock] case class Empty(properties: Properties[Unit]) extends Product[Unit]:
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def toChain: Chain[Eval[Schema[?]]] = Chain.empty
    override def description: Property.Optional[String] = Property.Optional(
      properties.description,
      f => copy(properties = properties.copy(description = f(properties.description)))
    )
    override def example: Property.Optional[Unit] = Property.Optional(
      properties.example,
      f => copy(properties = properties.copy(example = f(properties.example)))
    )

  final private[crock] case class One[A](schema: Eval[Schema[A]], properties: Properties[A]) extends Product[A]:
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def toChain: Chain[Eval[Schema[A]]] = Chain.one(schema)
    override def description: Property.Optional[String] = Property.Optional(
      properties.description,
      f => copy(properties = properties.copy(description = f(properties.description)))
    )
    override def example: Property.Optional[A] = Property.Optional(
      properties.example,
      f => copy(properties = properties.copy(example = f(properties.example)))
    )

  final private[crock] case class Zip[A, B](left: Product[A], right: Product[B], properties: Properties[(A, B)])
      extends Product[(A, B)]:
    override def constraints: Chain[Constraint] = left.constraints ++ right.constraints
    override def isOptional: Boolean = left.isOptional && right.isOptional
    override def toChain: Chain[Eval[Schema[?]]] = left.toChain ++ right.toChain
    override def description: Property.Optional[String] = Property.Optional(
      properties.description,
      f => copy(properties = properties.copy(description = f(properties.description)))
    )
    override def example: Property.Optional[(A, B)] = Property.Optional(
      properties.example,
      f => copy(properties = properties.copy(example = f(properties.example)))
    )

  final private[crock] case class Validate[A, B](self: Product[A], validation: Validation[A, B], g: B => A)
      extends Product[B]:
    export self.{isOptional, toChain}
    override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
    override def description: Property.Optional[String] =
      Property.Optional(self.description.value, f => copy(self = self.description.modify(f)))
    override def example: Property.Optional[B] =
      Property.Optional(self, _.example, value => copy(self = self.example(value)), validation, g)

  final private[crock] case class Optional[A](self: Product[A]) extends Product[Option[A]]:
    export self.{constraints, toChain}
    override def isOptional: Boolean = true
    override def description: Property.Optional[String] = Property.Optional(
      self.description.value,
      f => copy(self = self.description.modify(f))
    )
    override def example: Property.Optional[Option[A]] = Property.Optional(
      self.example.value.map(_.some),
      f => copy(self = self.example.modify(example => f(example.map(_.some)).flatten))
    )

  val empty: Product[Unit] = Empty(Properties.Empty)
  def apply[A](schema: => Schema[A]): Product[A] = One(Eval.later(schema), Properties.Empty)
