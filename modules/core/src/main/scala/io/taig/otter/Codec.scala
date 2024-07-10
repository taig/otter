package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation

trait Codec[+F[+_], +A, B] extends Codec.Reader[F, A, B], Codec.Writer[F, A, B]:
  def asReader: Codec.Reader[F, A, B] = this
  def asWriter: Codec.Writer[F, A, B] = this

  def imap[C](f: B => C)(g: C => B): Codec[F, A, C]
  def default(value: B): Codec[F, A, B]
  override def optional: Codec[F, A, Option[B]]

object Codec:
  trait Required[+F[+_], +A, B] extends Codec[F, A, B], Codec.Required.Reader[F, A, B], Codec.Required.Writer[F, A, B]:
    override def asReader: Codec.Required.Reader[F, A, B] = this
    override def asWriter: Codec.Required.Writer[F, A, B] = this

    override def imap[C](f: B => C)(g: C => B): Codec.Required[F, A, C]

  object Required:
    trait Reader[+F[+_], +A, +B] extends Codec.Reader[F, A, B]:
      def typeName: String

      override def map[C](f: B => C): Codec.Required.Reader[F, A, C]

      final override def decode(data: Option[Data.Value]): Result[B] = data
        .toValid(Violations.rootNec(Violation(Constraint.Type(typeName), actual = Data.String("null"))))
        .andThen(decode)

      def decode(data: Data.Value): Codec.Result[B]

    trait Writer[+F[+_], +A, -B] extends Codec.Writer[F, A, B]:
      override def contramap[C](f: C => B): Codec.Required.Writer[F, A, C]
      final override def encode(b: B): Option[Out] = encodeRequired(b).some
      def encodeRequired(b: B): Out

  trait Reader[+F[+_], +A, +B]:
    def map[C](f: B => C): Codec.Reader[F, A, C]
    def default[B1 >: B](value: B1): Codec.Reader[F, A, B1]
    def optional: Codec.Reader[F, A, Option[B]]
    final def decode(data: Data): Codec.Result[B] = decode(data.toValue)
    def decode(data: Option[Data.Value]): Codec.Result[B]

  trait Writer[+F[+_], +A, -B]:
    type Out <: Data.Value
    def contramap[C](f: C => B): Codec.Writer[F, A, C]
    def optional: Codec.Writer[F, A, Option[B]]
    def encode(b: B): Option[Out]

  type Result[A] = Validated[Violations[Constraint.Any[Data], Data], A]
