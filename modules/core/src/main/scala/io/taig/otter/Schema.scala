package io.taig.otter

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.validation.{Constraint, Validation, Violation, Violations}

abstract class Schema[A]:
  self =>
  type Self[a] <: Schema[a]

  def constraints: Chain[Constraint]
  def isOptional: Boolean

  def description: Option[String]
  def description(f: Option[String] => Option[String]): Self[A]
  final def description(value: Option[String]): Self[A] = description(_ => value)
  final def description(value: String): Self[A] = description(Some(value))

  def optional: Self[Option[A]]

  def ivalidate[B](validation: Validation[A, B])(g: B => A): Self[B]
  final def validate(validation: Validation[A, Unit]): Self[A] = ivalidate(validation.tap)(identity)
  final def imap[B](f: A => B)(g: B => A): Self[B] = ivalidate(Validation.lift(f))(g)

  final def :+[B](schema: Schema[B]): Union.Of[this.type | schema.type, Either[A, B]] = toUnion.orElse(schema.toUnion)
  final def +:[B](schema: Schema[B]): Union.Of[this.type | schema.type, Either[B, A]] = schema.toUnion.orElse(toUnion)

  def encode(a: A): Data
  final def decode(data: Data): Validated[Violations, A] = decode(data.asValue)
  def decode(data: Option[Data.Value]): Validated[Violations, A]

  final def toUnion: Union.Of[this.type, A] = Union(this)

object Schema:
  extension [A <: Matchable](self: Schema[A])
    inline def |[B <: Matchable](schema: Schema[B]): Union.Of[self.type | schema.type, A | B] = (self :+ schema).imap {
      case Left(a)  => a
      case Right(b) => b
    } {
      case a: A => Left(a)
      case b: B => Right(b)
    }
