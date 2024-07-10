package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Codec.Result
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation

trait Value[+F[+_], +A, B] extends Codec[F, A, B], Value.Reader[F, A, B], Value.Writer[F, A, B]:
  override def asReader: Value.Reader[F, A, B] = this
  override def asWriter: Value.Writer[F, A, B] = this
  override def default(value: B): Value[F, A, B]
  override def optional: Value[F, A, Option[B]]

object Value:
  trait Required[+F[+_], +A, B]
      extends Value[F, A, B],
        Codec.Required[F, A, B],
        Value.Required.Reader[F, A, B],
        Value.Required.Writer[F, A, B]:
    override def asReader: Value.Required.Reader[F, A, B] = this
    override def asWriter: Value.Required.Writer[F, A, B] = this
    override def imap[C](f: B => C)(g: C => B): Value.Required[F, A, C]

  object Required:
    trait Reader[+F[+_], +A, +B] extends Value.Reader[F, A, B], Codec.Required.Reader[F, A, B]:
      override def map[C](f: B => C): Value.Required.Reader[F, A, C]
      final override def parse(value: Option[String]): Codec.Result[B] = value
        .toValid(Violations.rootNec(Violation(Constraint.Type(typeName), actual = Data.String("null"))))
        .andThen(parse)
      def parse(value: String): Codec.Result[B]

    trait Writer[+F[+_], +A, -B] extends Value.Writer[F, A, B], Codec.Required.Writer[F, A, B]:
      override def contramap[C](f: C => B): Value.Required.Writer[F, A, C]
      final override def print(b: B): Option[String] = printRequired(b).some
      def printRequired(b: B): String

  trait Reader[+F[+_], +A, +B] extends Codec.Reader[F, A, B]:
    override def default[B1 >: B](value: B1): Value.Reader[F, A, B1]
    override def map[C](f: B => C): Value.Reader[F, A, C]
    override def optional: Value.Reader[F, A, Option[B]]
    def parse(value: Option[String]): Codec.Result[B]

  trait Writer[+F[+_], +A, -B] extends Codec.Writer[F, A, B]:
    final override type Out = Data.Primitive
    override def contramap[C](f: C => B): Value.Writer[F, A, C]
    override def optional: Value.Writer[F, A, Option[B]]
    def print(b: B): Option[String]
