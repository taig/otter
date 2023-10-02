package io.taig.otter

import cats.syntax.all.*
import cats.data.{Chain, NonEmptyChain, Validated}
import io.taig.otter.validation.{Constraint, Validation, Violations}

sealed abstract class Union[A](description: Option[String]) extends Schema[A](description):
  self =>
  override type Self[a] = Union.Of[Of, a]
  type Of <: Schema[?]

  def toNonEmptyChain: NonEmptyChain[Schema[?]]

  final override def description(f: Option[String] => Option[String]): Union.Of[Of, A] = Union(this, f(description))

  final override def optional: Union.Of[Of, Option[A]] = new Union[Option[A]](description):
    export self.{constraints, toNonEmptyChain, Of}
    override def isOptional: Boolean = true
    override def encode(a: Option[A]): Data = a.map(self.encode).getOrElse(Data.Null)
    override def decode(data: Option[Data.Value]): Validated[Violations, Option[A]] =
      data.fold(none.valid)(_ => self.decode(data).map(_.some))

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Union.Of[Of, B] = new Union[B](description):
    export self.{isOptional, toNonEmptyChain, Of}
    override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
    override def encode(b: B): Data = self.encode(g(b))
    override def decode(data: Option[Data.Value]): Validated[Violations, B] =
      self.decode(data).andThen(validation(_).leftMap(Violations.root))

  final def orElse[B](schema: Union[B]): Union.Of[self.Of | schema.Of, Either[A, B]] = new Union[Either[A, B]](None):
    override type Of = self.Of | schema.Of
    override def toNonEmptyChain: NonEmptyChain[Schema[?]] = self.toNonEmptyChain.concat(schema.toNonEmptyChain)
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def encode(ab: Either[A, B]): Data = ab.fold(self.encode, schema.encode)
    override def decode(data: Option[Data.Value]): Validated[Violations, Either[A, B]] =
      self.decode(data).map(_.asLeft).findValid(schema.decode(data).map(_.asRight))

object Union:
  type Of[A <: Schema[?], B] = Union[B] { type Of <: A }

  def apply[A](schema: Union[A], description: Option[String]): Union.Of[schema.Of, A] =
    new Union[A](description) { export schema.* }

  def apply[A](schema: Schema[A]): Union.Of[schema.type, A] = new Union[A](None):
    override type Of = schema.type
    override def toNonEmptyChain: NonEmptyChain[Schema[?]] = NonEmptyChain.one(schema)
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def encode(a: A): Data = schema.encode(a)
    override def decode(data: Option[Data.Value]): Validated[Violations, A] = schema.decode(data)
