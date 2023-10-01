package io.taig.otter

import cats.data.{Chain, Validated}
import io.taig.otter.validation.{Constraint, Validation, Violation, Violations}

abstract class Value[A](description: Option[String]) extends Schema[A](description):
  self =>
  override type Self[a] <: Value[a]

  final def orElse[B](schema: Value[B]): Value[Either[A, B]] = new Value.Root[Either[A, B]](None):
    override def print(a: Either[A, B]): Option[String] = ???
    override def parse(value: Option[String]): Validated[Violations, Either[A, B]] = ???
    override def constraints: Chain[Constraint] = ???
    override def isOptional: Boolean = ???
    override def encode(a: Either[A, B]): Data = ???

    override def decode(data: Option[Data.Value]): Validated[Violations, Either[A, B]] = ???

  final def :+[B](schema: Value[B]): Value[Either[A, B]] = orElse(schema)
  final def +:[B](schema: Value[B]): Value[Either[B, A]] = schema.orElse(this)

  def print(a: A): Option[String]
  def parse(value: Option[String]): Validated[Violations, A]

object Value:
  extension [A <: Matchable](self: Value[A])
    inline def |[B <: Matchable](schema: Value[B]): Value[A | B] = self
      .orElse(schema)
      .imap {
        case Left(a)  => a
        case Right(b) => b
      } {
        case a: A => Left(a)
        case b: B => Right(b)
      }

  def apply[A](schema: Value[A], description: Option[String]): Value[A] = new Value[A](description) { export schema.* }

  abstract private class Root[A](description: Option[String]) extends Value[A](description):
    self =>
    final override type Self[a] = Value[a]

    final override def description(f: Option[String] => Option[String]): Value[A] = Value(this, f(description))

    final override def optional: Value[Option[A]] = ???

    final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Value[B] =
      new Root[B](description):
        export self.isOptional

        override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints

        override def decode(data: Option[Data.Value]): Validated[Violations, B] =
          self.decode(data).andThen(validation(_).leftMap(Violations.root))

        override def encode(b: B): Data = self.encode(g(b))

        override def parse(value: Option[String]): Validated[Violations, B] =
          self.parse(value).andThen(validation(_).leftMap(Violations.root))

        override def print(b: B): Option[String] = self.print(g(b))
