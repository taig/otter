package io.taig.crock.schema

import cats.Eval
import cats.data.Chain
import io.taig.crock.validation.{Constraint, Validation}

sealed abstract class Collection[A] extends Schema[A]:
  self =>
  override type Self[a] = Collection.Of[Of, a]
  type Of[a] <: Schema[a]
  def of: Eval[Schema[?]]

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

  final case class Validate[F[a] <: Schema[a], A, B](
      collection: Collection.Of[F, A],
      validation: Validation[A, B],
      g: B => A
  ) extends Collection[B]:
    export collection.{of, Of}
    override def constraints: Chain[Constraint] = collection.constraints ++ validation.constraints
    override def description: Property.Optional[String] =
      Property.Optional(collection, _.description, value => copy(collection = collection.description(value)))
    override def example: Property.Optional[B] =
      Property.Optional(collection, _.example, value => copy(collection = collection.example(value)), validation, g)

  def apply[F[a] <: Schema[a], A](schema: Eval[F[A]]): Collection.Of[F, Vector[A]] = Root(schema, Properties.Empty)
