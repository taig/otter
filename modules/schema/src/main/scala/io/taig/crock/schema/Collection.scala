package io.taig.crock.schema

import cats.Eval
import cats.data.Chain
import io.taig.crock.validation.{Constraint, Validation}
import monocle.syntax.all.*

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
    override def description: Property.Optional[String] =
      Property.Optional(properties.description, this.focus(_.properties.description).modify)
    override def example: Property.Optional[Vector[A]] =
      Property.Optional(properties.example, this.focus(_.properties.example).modify)

  final case class Validate[F[a] <: Schema[a], A, B](
      schema: Collection.Of[F, A],
      validation: Validation[A, B],
      g: B => A
  ) extends Collection[B]:
    export schema.{of, Of}
    override def constraints: Chain[Constraint] = schema.constraints ++ validation.constraints
    override def description: Property.Optional[String] =
      Property.Optional(schema, _.description, value => copy(schema = schema.description(value)))
    override def example: Property.Optional[B] =
      Property.Optional(schema, _.example, value => copy(schema = schema.example(value)), validation, g)

  def apply[F[a] <: Schema[a], A](schema: Eval[F[A]]): Collection.Of[F, Vector[A]] = Root(schema, Properties.Empty)
