package io.taig.otter.component

import cats.Invariant
import cats.syntax.all.*
import io.taig.otter.Constraint
import io.taig.otter.Reference
import io.taig.otter.operation.PrimitiveOperation
import io.taig.validation.Validation

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean
import scala.annotation.targetName

object PrimitiveComponent:
  trait Boolean[F[_]](using F: PrimitiveOperation.Boolean[F]):
    val boolean: F[SBoolean] = F.boolean

  object Coerce:
    trait Boolean[F[_], G[_]](using F: PrimitiveOperation.Coerce.Boolean[F, G]):
      @targetName("coerceBoolean")
      def coerce[A](schema: => G[A]): F[A] = F.lift(schema = Reference.later(schema))

    object Boolean:
      trait Read[F[_], G[_]](using F: PrimitiveOperation.Coerce.Boolean.Read[F, G]):
        @targetName("coerceBooleanRead")
        def coerce[A](schema: => G[A]): F[A] = F.lift(schema = Reference.later(schema))

      trait Write[F[_], G[_]](using F: PrimitiveOperation.Coerce.Boolean.Write[F, G]):
        @targetName("coerceBooleanWrite")
        def coerce[A](schema: => G[A]): F[A] = F.lift(schema = Reference.later(schema))

    trait Number[F[_], G[_]](using F: PrimitiveOperation.Coerce.Number[F, G]):
      @targetName("coerceNumber")
      def coerce[A](schema: => G[A]): F[A] = F.lift(schema = Reference.later(schema))

    object Number:
      trait Read[F[_], G[_]](using F: PrimitiveOperation.Coerce.Number.Read[F, G]):
        @targetName("coerceNumberRead")
        def coerce[A](schema: => G[A]): F[A] = F.lift(schema = Reference.later(schema))

      trait Write[F[_], G[_]](using F: PrimitiveOperation.Coerce.Number.Write[F, G]):
        @targetName("coerceNumberWrite")
        def coerce[A](schema: => G[A]): F[A] = F.lift(schema = Reference.later(schema))

    trait Text[F[_], G[_]](using F: PrimitiveOperation.Coerce.Text[F, G]):
      @targetName("coerceText")
      def coerce[A](schema: => G[A]): F[A] = F.lift(schema = Reference.later(schema))

    object Text:
      trait Read[F[_], G[_]](using F: PrimitiveOperation.Coerce.Text.Read[F, G]):
        @targetName("coerceTextRead")
        def coerce[A](schema: => G[A]): F[A] = F.lift(schema = Reference.later(schema))

      trait Write[F[_], G[_]](using F: PrimitiveOperation.Coerce.Text.Write[F, G]):
        @targetName("coerceTextWrite")
        def coerce[A](schema: => G[A]): F[A] = F.lift(schema = Reference.later(schema))

  trait Number[F[_]](using F: PrimitiveOperation.Number[F]):
    def jBigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): F[JBigDecimal] =
      F.bigDecimal(validation)

    val jBigDecimal: F[JBigDecimal] = jBigDecimal(validation = Validation.valid)

    def bigDecimal(validation: Validation[Constraint.Primitive.Number, BigDecimal])(using Invariant[F]): F[BigDecimal] =
      jBigDecimal(validation = validation.contramap(BigDecimal.apply)).imap(BigDecimal.apply)(_.bigDecimal)

    def bigDecimal(using Invariant[F]): F[BigDecimal] = bigDecimal(validation = Validation.valid)

    def jBigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger]): F[JBigInteger] =
      F.bigInteger(validation)

    val jBigInteger: F[JBigInteger] = jBigInteger(validation = Validation.valid)

    def bigInt(validation: Validation[Constraint.Primitive.Number, BigInt])(using Invariant[F]): F[BigInt] =
      jBigInteger(validation = validation.contramap(BigInt.apply)).imap(BigInt.apply)(_.bigInteger)

    def bigInt(using Invariant[F]): F[BigInt] = bigInt(validation = Validation.valid)

    def double(validation: Validation[Constraint.Primitive.Number, Double]): F[Double] = F.double(validation)

    val double: F[Double] = double(validation = Validation.valid)

    def float(validation: Validation[Constraint.Primitive.Number, Float]): F[Float] = F.float(validation)

    val float: F[Float] = float(validation = Validation.valid)

    def int(validation: Validation[Constraint.Primitive.Number, Int]): F[Int] = F.int(validation)

    val int: F[Int] = int(validation = Validation.valid)

    def long(validation: Validation[Constraint.Primitive.Number, Long]): F[Long] = F.long(validation)

    val long: F[Long] = long(validation = Validation.valid)

  trait Text[F[_]](using F: PrimitiveOperation.Text[F]):
    def codec[A](name: String, parse: String => Either[String, A], print: A => String): F[A] =
      F.codec(name, parse, print)

    def string(validation: Validation[Constraint.Primitive.Text, String]): F[String] = F.string(validation)

    val string: F[String] = string(validation = Validation.valid)

  object Text:
    trait Read[F[_]](using F: PrimitiveOperation.Text.Read[F]):
      def parser[A](name: String, parse: String => Either[String, A]): F[A] = F.parser(name, parse)

    trait Write[F[_]](using F: PrimitiveOperation.Text.Write[F]):
      def printer[A](name: String, print: A => String): F[A] = F.printer(name, print)
