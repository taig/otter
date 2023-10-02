package io.taig.otter

import cats.data.{Chain, NonEmptyChain, Validated}
import io.taig.otter.validation.{Constraint, Validation, Violations}

sealed abstract class Union[A](description: Option[String]) extends Schema[A](description) {
  self =>
  override type Self[a] = Union.Of[Of, a]
  type Of <: Schema[?]

  def toNonEmptyChain: NonEmptyChain[Schema[?]]

  final override def description(f: Option[String] => Option[String]): Union.Of[Of, A] = ???

  final override def optional: Union.Of[Of, Option[A]] = ???

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Union.Of[Of, B] = ???

  def orElse[B](schema: Union[B]): Union.Of[self.Of | schema.Of, Either[A, B]] = ???
}

object Union:
  type Of[A <: Schema[?], B] = Union[B] { type Of <: A }

  def apply[A](schema: Schema[A]): Union.Of[schema.type, A] = new Union[A](None):
    override type Of = schema.type
    override def toNonEmptyChain: NonEmptyChain[Schema[?]] = NonEmptyChain.one(schema)
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def encode(a: A): Data = schema.encode(a)
    override def decode(data: Option[Data.Value]): Validated[Violations, A] = schema.decode(data)
