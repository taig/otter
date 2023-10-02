package io.taig.otter

import cats.syntax.all.*
import cats.data.{Chain, NonEmptyChain, Validated}
import io.taig.otter.validation.{Constraint, Validation, Violations}

sealed abstract class Union[A] extends Schema[A]:
  self =>
  override type Self[a] <: Union.Of[Of, a]
  final override type Optional[a] = Union.Of[Of, a]
  type Of <: Schema[?]

  def toNonEmptyChain: NonEmptyChain[Schema[?]]

  final override def optional: Union.Of[Of, Option[A]] = ???

  final def orElse[B](schema: Union[B]): Union.Of[self.Of | schema.Of, Either[A, B]] =
    new Union.Root[Either[A, B]](None):
      override type Of = self.Of | schema.Of
      override def toNonEmptyChain: NonEmptyChain[Schema[?]] = self.toNonEmptyChain.concat(schema.toNonEmptyChain)
      override def constraints: Chain[Constraint] = Chain.empty
      override def isOptional: Boolean = false
      override def decode(data: Option[Data.Value]): Validated[Violations, Either[A, B]] =
        self.decode(data).map(_.asLeft).findValid(schema.decode(data).map(_.asRight))
      override def encode(ab: Either[A, B]): Data = ab.fold(self.encode, schema.encode)
      override def parse(value: Option[String])(using Of <:< Value[?]): Validated[Violations, Either[A, B]] =
        self.parse(value).map(_.asLeft).findValid(schema.parse(value).map(_.asRight))
      override def print(ab: Either[A, B])(using Of <:< Value[?]): Option[String] = ab.fold(self.print, schema.print)

  def parse(value: Option[String])(using Of <:< Value[?]): Validated[Violations, A]
  def print(a: A)(using Of <:< Value[?]): Option[String]

object Union:
  type Of[A <: Schema[?], B] = Union[B] { type Of <: A }

  sealed abstract class Required[A](val description: Option[String]) extends Union[A]:
    self =>
    final override type Self[a] = Union.Required.Of[Of, a]
    override type Of <: Value.Required[?]
    final override def isOptional: Boolean = false
    final override def description(f: Option[String] => Option[String]): Required.Of[Of, A] =
      new Required[A](f(description)) { export self.* }
    final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Required.Of[Of, B] =
      new Required[B](description):
        export self.{toNonEmptyChain, Of}
        override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
        override def decode(data: Option[Data.Value]): Validated[Violations, B] =
          self.decode(data).andThen(validation(_).leftMap(Violations.root))
        override def encode(b: B): Data = self.encode(g(b))
        override def parse(value: Option[String])(using Of <:< Value[?]): Validated[Violations, B] =
          self.parse(value).andThen(validation(_).leftMap(Violations.root))
        override def print(b: B): String = self.print(g(b))
    final def orElse[B](schema: Union.Required[B]): Union.Required.Of[self.Of | schema.Of, Either[A, B]] =
      new Required[Either[A, B]](None):
        export self.Of
        override def toNonEmptyChain: NonEmptyChain[Schema[?]] = self.toNonEmptyChain.concat(schema.toNonEmptyChain)
        override def constraints: Chain[Constraint] = Chain.empty
        override def decode(data: Option[Data.Value]): Validated[Violations, Either[A, B]] =
          self.decode(data).map(_.asLeft).findValid(schema.decode(data).map(_.asRight))
        override def encode(ab: Either[A, B]): Data = ab.fold(self.encode, schema.encode)
        override def parse(value: Option[String])(using Of <:< Value[?]): Validated[Violations, Either[A, B]] =
          self.parse(value).map(_.asLeft).findValid(schema.parse(value).map(_.asRight))
        override def print(ab: Either[A, B]): String = ab.fold(self.print, schema.print)
    final override def print(a: A)(using Of <:< Value[?]): Option[String] = print(a).some

    final def parse(value: String): Validated[Violations, A] = parse(Some(value))
    def print(a: A): String

  object Required:
    type Of[A <: Value.Required[?], B] = Union.Required[B] { type Of <: A }

    def apply[A](schema: Value.Required[A]): Union.Required.Of[schema.type, A] = new Required[A](None):
      override type Of = schema.type
      override def toNonEmptyChain: NonEmptyChain[Schema[?]] = NonEmptyChain.one(schema)
      override def constraints: Chain[Constraint] = Chain.empty
      override def decode(data: Option[Data.Value]): Validated[Violations, A] = schema.decode(data)
      override def encode(a: A): Data = schema.encode(a)
      override def parse(value: Option[String])(using schema.type <:< Value[?]): Validated[Violations, A] =
        schema.parse(value)
      override def print(a: A): String = schema.print(a)

  abstract private class Root[A](val description: Option[String]) extends Union[A]:
    self =>
    override type Self[a] = Union.Of[self.Of, a]
    override def description(f: Option[String] => Option[String]): Union.Of[self.Of, A] =
      new Root[A](f(description)) { export self.* }
    override def ivalidate[B](validation: Validation[A, B])(g: B => A): Union.Of[self.Of, B] =
      new Root[B](description) {
        export self.{isOptional, toNonEmptyChain, Of}
        override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
        override def decode(data: Option[Data.Value]): Validated[Violations, B] =
          self.decode(data).andThen(validation(_).leftMap(Violations.root))
        override def encode(b: B): Data = self.encode(g(b))
        override def parse(value: Option[String])(using Of <:< Value[?]): Validated[Violations, B] =
          self.parse(value).andThen(validation(_).leftMap(Violations.root))
        override def print(b: B)(using Of <:< Value[?]): Option[String] = self.print(g(b))
      }

  def apply[A](schema: Schema[A]): Union.Of[schema.type, A] = new Root[A](None):
    override type Of = schema.type
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def toNonEmptyChain: NonEmptyChain[Schema[?]] = NonEmptyChain.one(schema)
    override def decode(data: Option[Data.Value]): Validated[Violations, A] = schema.decode(data)
    override def encode(a: A): Data = schema.encode(a)
    override def parse(value: Option[String])(using schema.type <:< Value[?]): Validated[Violations, A] =
      schema.asInstanceOf[Value[A]].parse(value)
    override def print(a: A)(using schema.type <:< Value[?]): Option[String] =
      schema.asInstanceOf[Value[A]].print(a) match
        case value: String         => value.some
        case value: Option[String] => value
