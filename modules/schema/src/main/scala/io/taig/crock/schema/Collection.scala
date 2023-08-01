package io.taig.crock.schema

import cats.Eval
import cats.data.Chain
import io.taig.crock.validation.{Constraint, Validation}
import monocle.syntax.all.*

sealed abstract class Collection[A] extends Schema[A]:
  self =>
  override type Self[a] = Collection.Of[a, Of]
  type Of <: Schema[?]
  def of: Eval[Of]

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Collection.Of[B, Of] =
    Collection.Validate(this, validation, g)

object Collection:
  type Of[A, B <: Schema[?]] = Collection[A] { type Of = B }

  final case class Properties[+A](description: Option[String], example: Option[A])

  object Properties:
    val Empty: Collection.Properties[Nothing] = Properties(None, None)

  final case class Root[A <: Schema[B], B](of: Eval[A], properties: Properties[Vector[B]])
      extends Collection[Vector[B]]:
    override type Of = A
    override def constraints: Chain[Constraint] = Chain.empty
    override def description: Property.Optional[String] =
      Property.Optional(properties.description, this.focus(_.properties.description).modify)
    override def example: Property.Optional[Vector[B]] =
      Property.Optional(properties.example, this.focus(_.properties.example).modify)

  final case class Validate[A, B, C <: Schema[?]](
      schema: Collection.Of[A, C],
      validation: Validation[A, B],
      g: B => A
  ) extends Collection[B]:
    export schema.{of, Of}
    override def constraints: Chain[Constraint] = schema.constraints ++ validation.constraints
    override def description: Property.Optional[String] =
      Property.Optional(schema, _.description, value => copy(schema = schema.description(value)))
    override def example: Property.Optional[B] =
      Property.Optional(schema, _.example, value => copy(schema = schema.example(value)), validation, g)

  def apply[A <: Schema[B], B](schema: Eval[A]): Collection.Of[Vector[B], A] = Root(schema, Properties.Empty)
