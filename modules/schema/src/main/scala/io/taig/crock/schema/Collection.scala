package io.taig.crock.schema

import cats.Eval
import cats.data.Chain
import cats.syntax.all.*
import io.taig.crock.validation.{Constraint, Validation}

sealed abstract class Collection[A] extends Schema[A]:
  self =>
  override type Self[a] = Collection.Of[Of, a]
  type Of[a] <: Schema[a]
  def of: Eval[Schema[?]]

  final override def optional: Collection.Of[Of, Option[A]] = Collection.Optional(this)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Collection.Of[Of, B] =
    Collection.Validate(this, validation, g)

object Collection:
  type Of[F[a] <: Schema[a], A] = Collection[A] { type Of[a] = F[a] }

  final case class Properties[+A](description: Option[String], example: Option[A])

  object Properties:
    val Empty: Collection.Properties[Nothing] = Properties(None, None)

  final case class Root[F[a] <: Schema[a], A](of: Eval[F[A]], properties: Properties[Vector[A]])
      extends Collection[Vector[A]]:
    override type Of[a] = F[a]
    override def constraints: Chain[Constraint] = Chain.empty
    override def description: Property.Optional[String] = Property.Optional(
      properties.description,
      f => copy(properties = properties.copy(description = f(properties.description)))
    )
    override def example: Property.Optional[Vector[A]] = Property.Optional(
      properties.example,
      f => copy(properties = properties.copy(example = f(properties.example)))
    )

  final case class Optional[F[a] <: Schema[a], A](self: Collection.Of[F, A]) extends Collection[Option[A]]:
    export self.{constraints, of, Of}
    override def description: Property.Optional[String] = Property.Optional(
      self.description.value,
      f => copy(self = self.description.modify(f))
    )
    override def example: Property.Optional[Option[A]] = Property.Optional(
      self.example.value.map(_.some),
      f => copy(self = self.example.modify(example => f(example.map(_.some)).flatten))
    )

  final case class Validate[F[a] <: Schema[a], A, B](
      self: Collection.Of[F, A],
      validation: Validation[A, B],
      g: B => A
  ) extends Collection[B]:
    export self.{of, Of}
    override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
    override def description: Property.Optional[String] =
      Property.Optional(self, _.description, value => copy(self = self.description(value)))
    override def example: Property.Optional[B] =
      Property.Optional(self, _.example, value => copy(self = self.example(value)), validation, g)

  def apply[F[a] <: Schema[a], A](schema: Eval[F[A]]): Collection.Of[F, Vector[A]] = Root(schema, Properties.Empty)
