package io.taig.otter

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.validation.{Constraint, Validation, Violation, Violations}

abstract class Schema[A](val description: Option[String]):
  self =>
  type Self[a] <: Schema[a]

  def constraints: Chain[Constraint]
  def isOptional: Boolean

  def description(f: Option[String] => Option[String]): Self[A]
  final def description(value: Option[String]): Self[A] = description(_ => value)
  final def description(value: String): Self[A] = description(Some(value))

  def optional: Self[Option[A]]

  def ivalidate[B](validation: Validation[A, B])(g: B => A): Self[B]
  final def validate(validation: Validation[A, Unit]): Self[A] = ivalidate(validation.tap)(identity)
  final def imap[B](f: A => B)(g: B => A): Self[B] = ivalidate(Validation.lift(f))(g)

  final def orElse[B](schema: Schema[B]): Schema[Either[A, B]] = new Schema.Root[Either[A, B]]:
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def encode(ab: Either[A, B]): Data = ab.fold(self.encode, schema.encode)
    override def decode(data: Data): Validated[Violations, Either[A, B]] =
      self.decode(data).map(_.asLeft).findValid(schema.decode(data).map(_.asRight))
  final def :+[B](schema: Schema[B]): Schema[Either[A, B]] = orElse(schema)
  final def +:[B](schema: Schema[B]): Schema[Either[B, A]] = schema.orElse(this)

  def encode(a: A): Data
  def decode(data: Data): Validated[Violations, A]

object Schema:
  extension [A <: Matchable](self: Schema[A])
    inline def |[B <: Matchable](schema: Schema[B]): Schema[A | B] = self
      .orElse(schema)
      .imap {
        case Left(a)  => a
        case Right(b) => b
      } {
        case a: A => Left(a)
        case b: B => Right(b)
      }

  def apply[A](schema: Schema[A], description: Option[String]): Schema[A] =
    new Schema[A](description) { export schema.* }

  sealed abstract class Root[A] extends Schema[A](None):
    self =>
    final override type Self[a] = Schema[a]
    final override def description(f: Option[String] => Option[String]): Schema[A] = Schema(this, f(description))
    final override def optional: Schema[Option[A]] = new Root[Option[A]]:
      export self.constraints
      override def isOptional: Boolean = true
      override def encode(a: Option[A]): Data = a.map(self.encode).getOrElse(Data.Null)
      override def decode(data: Data): Validated[Violations, Option[A]] = data match
        case Data.Null => none.valid
        case _         => self.decode(data).map(_.some)
    final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Schema[B] = new Root[B]:
      export self.isOptional
      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
      override def encode(b: B): Data = self.encode(g(b))
      override def decode(data: Data): Validated[Violations, B] =
        self.decode(data).andThen(validation(_).leftMap(Violations.root))
