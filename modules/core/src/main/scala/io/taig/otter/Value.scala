package io.taig.otter

import cats.data.Validated
import io.taig.otter.validation.{Violation, Violations}

trait Value[A] extends Codec[A]:
  override type Self[a] <: Value[a]
  override type Optional[a] <: Value[a]

  def print(a: A): String | Option[String]
  def parse(value: Option[String]): Validated[Violations, A]

object Value:
  trait Required[A] extends Value[A]:
    override type Self[a] <: Value.Required[a]

    final def :+[B](codec: Value.Required[B]): Union.Required.Of[this.type | codec.type, Either[A, B]] =
      toUnion.orElse(codec.toUnion)
    final def +:[B](codec: Value.Required[B]): Union.Required.Of[this.type | codec.type, Either[B, A]] =
      codec.toUnion.orElse(toUnion)

    final override def toUnion: Union.Required.Of[this.type, A] = Union.Required(this)

    override def encode(a: A): Data.Primitive
    override def print(a: A): String
    final override def parse(value: Option[String]): Validated[Violations, A] =
      Validated.fromOption(value, Violations.rootNec(Violation.required)).andThen(parse)
    def parse(value: String): Validated[Violations, A]

  object Required:
    extension [A <: Matchable](self: Value.Required[A])
      inline def |[B <: Matchable](codec: Value.Required[B]): Union.Required.Of[self.type | codec.type, A | B] =
        (self :+ codec).imap {
          case Left(a)  => a
          case Right(b) => b
        } {
          case a: A => Left(a)
          case b: B => Right(b)
        }
