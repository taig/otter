package io.taig.otter

import cats.data.Validated
import io.taig.otter.validation.{Violation, Violations}

trait Value[A] extends Schema[A]:
  override type Self[a] <: Value[a]
  override type Optional[a] <: Value[a]

  def print(a: A): String | Option[String]
  def parse(value: Option[String]): Validated[Violations, A]

object Value:
  trait Required[A] extends Value[A]:
    override type Self[a] <: Value.Required[a]

    final def :+[B](schema: Value.Required[B]): Union.Required.Of[this.type | schema.type, Either[A, B]] =
      toUnion.orElse(schema.toUnion)
    final def +:[B](schema: Value.Required[B]): Union.Required.Of[this.type | schema.type, Either[B, A]] =
      schema.toUnion.orElse(toUnion)

    final override def toUnion: Union.Required.Of[this.type, A] = Union.Required(this)

    override def encode(a: A): Data.Primitive
    override def print(a: A): String
    final override def parse(value: Option[String]): Validated[Violations, A] =
      Validated.fromOption(value, Violations.rootNec(Violation.required)).andThen(parse)
    def parse(value: String): Validated[Violations, A]
