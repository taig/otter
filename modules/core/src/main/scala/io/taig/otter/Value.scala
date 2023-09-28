package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.validation.{Validation, Violations}

trait Value[A] extends Schema[A]:
  self =>
  override type Self[a] <: Value[a]

  final def orElse[B](schema: Value[B]): Value[Either[A, B]] = ???
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

  def apply[A](schema: Value[A], description: Option[String]): Value[A] =
    new Schema[A](description) with Value[A] { export schema.* }

  sealed abstract class Root[A] extends Schema[A](None) with Value[A]:
    self =>
    final override type Self[a] = Value[a]
    final override def description(f: Option[String] => Option[String]): Value[A] = Value(this, f(description))
    final override def optional: Value[Option[A]] = ???
    final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Value[B] = ???
