package io.taig.otter

import cats.syntax.all.*
import cats.data.{Chain, Validated}
import io.taig.otter.validation.{Constraint, Validation, Violations}

abstract class Value[A](description: Option[String]) extends Schema[A](description):
  self =>
  override type Self[a] <: Value[a]

  final def orElse[B](schema: Value[B]): Value[Either[A, B]] = new Value.Root:
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decode(data: Data): Validated[Violations, Either[A, B]] =
      self.decode(data).map(_.asLeft).findValid(schema.decode(data).map(_.asRight))
    override def encode(ab: Either[A, B]): Data = ab.fold(self.encode, schema.encode)
    override def parse(value: Option[String]): Validated[Violations, Either[A, B]] =
      self.parse(value).map(_.asLeft).findValid(schema.parse(value).map(_.asRight))
    override def print(ab: Either[A, B]): Option[String] = ab.fold(self.print, schema.print)
  final def :+[B](schema: Value[B]): Value[Either[A, B]] = orElse(schema)
  final def +:[B](schema: Value[B]): Value[Either[B, A]] = schema.orElse(this)

  def print(a: A): Option[String]
  def parse(value: Option[String]): Validated[Violations, A]

object Value:
  def apply[A](schema: Value[A], description: Option[String]): Value[A] = new Value[A](description) { export schema.* }

  abstract class Root[A] extends Value[A](None):
    self =>
    final override type Self[a] = Value[a]
    final override def description(f: Option[String] => Option[String]): Value[A] = Value(this, f(description))
    final override def optional: Value[Option[A]] = new Root[Option[A]]:
      override def constraints: Chain[Constraint] = self.constraints
      override def isOptional: Boolean = true
      override def decode(data: Data): Validated[Violations, Option[A]] = data match
        case Data.Null => none.valid
        case _         => self.decode(data).map(_.some)
      override def encode(a: Option[A]): Data = a.map(self.encode).getOrElse(Data.Null)
      override def parse(value: Option[String]): Validated[Violations, Option[A]] =
        value.fold(none.valid)(_ => self.parse(value).map(_.some))
      override def print(a: Option[A]): Option[String] = a.flatMap(self.print)
    final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Value[B] = new Root[B]:
      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
      override def isOptional: Boolean = self.isOptional
      override def decode(data: Data): Validated[Violations, B] =
        self.decode(data).andThen(validation(_).leftMap(Violations.root))
      override def encode(b: B): Data = self.encode(g(b))
      override def parse(value: Option[String]): Validated[Violations, B] =
        self.parse(value).andThen(validation(_).leftMap(Violations.root))
      override def print(b: B): Option[String] = self.print(g(b))
