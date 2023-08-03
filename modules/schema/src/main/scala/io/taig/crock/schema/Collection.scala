package io.taig.crock.schema

import cats.Eval
import cats.data.Chain
import cats.syntax.all.*
import io.taig.crock.validation.{Constraint, Validation}

sealed abstract class Collection[Of[a] <: Schema[a], A] extends Schema[A]:
  override type Self[a] = Collection[Of, a]

  // Nasty, but unfortunately necessary as described in https://docs.scala-lang.org/scala3/guides/migration/incompat-other-changes.html#wildcard-type-argument
  final class Reference[B](val reference: Eval[Of[B]]):
    def value: Of[B] = reference.value
  def of: Reference[?] // TODO def of: Eval[Of[?]] -> unreducible application of higher-kinded type Collection.this.Of to wildcard arguments :-(

  final override def optional: Collection[Of, Option[A]] = Collection.Optional(this)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Collection[Of, B] =
    Collection.Validate(this, validation, g)

object Collection:
  final private[crock] case class Properties[+A](description: Option[String], example: Option[A])

  object Properties:
    val Empty: Collection.Properties[Nothing] = Properties(None, None)

  final private[crock] case class Root[Of[a] <: Schema[a], A](schema: Eval[Of[A]], properties: Properties[Chain[A]])
      extends Collection[Of, Chain[A]]:
    override def of: Reference[A] = new Reference(schema)
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def description: Property.Optional[String] = Property.Optional(
      properties.description,
      f => copy(properties = properties.copy(description = f(properties.description)))
    )
    override def example: Property.Optional[Chain[A]] = Property.Optional(
      properties.example,
      f => copy(properties = properties.copy(example = f(properties.example)))
    )

  final private[crock] case class Validate[Of[a] <: Schema[a], A, B](
      self: Collection[Of, A],
      validation: Validation[A, B],
      g: B => A
  ) extends Collection[Of, B]:
    export self.isOptional
    override def of: Reference[?] = new Reference(self.of.reference)
    override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
    override def description: Property.Optional[String] =
      Property.Optional(self.description.value, f => copy(self = self.description.modify(f)))
    override def example: Property.Optional[B] =
      Property.Optional(self, _.example, value => copy(self = self.example(value)), validation, g)

  final private[crock] case class Optional[Of[a] <: Schema[a], A](self: Collection[Of, A])
      extends Collection[Of, Option[A]]:
    export self.constraints
    override def of: Reference[?] = new Reference(self.of.reference)
    override def isOptional: Boolean = true
    override def description: Property.Optional[String] = Property.Optional(
      self.description.value,
      f => copy(self = self.description.modify(f))
    )
    override def example: Property.Optional[Option[A]] = Property.Optional(
      self.example.value.map(_.some),
      f => copy(self = self.example.modify(example => f(example.map(_.some)).flatten))
    )

  def apply[Of[a] <: Schema[a], A](schema: Eval[Of[A]]): Collection[Of, Chain[A]] = Root(schema, Properties.Empty)
