package io.taig.otter.component

import cats.arrow.Profunctor
import io.taig.otter.Constraint
import io.taig.otter.operation.PrimitiveOperation
import io.taig.validation.Validation

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean

object PrimitiveComponent:
  trait Boolean[F[-_, +_]](using F: PrimitiveOperation.Boolean[F]):
    val boolean: F[SBoolean, SBoolean] = F.boolean

  trait Number[F[-_, +_]](using F: PrimitiveOperation.Number[F]):
    def jBigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): F[JBigDecimal, JBigDecimal] =
      F.bigDecimal(validation)

    val jBigDecimal: F[JBigDecimal, JBigDecimal] = jBigDecimal(Validation.valid)

    def bigDecimal(validation: Validation[Constraint.Primitive.Number, BigDecimal])(using
        P: Profunctor[F]
    ): F[BigDecimal, BigDecimal] =
      P.dimap(jBigDecimal(validation.contramap(BigDecimal.apply)))((_: BigDecimal).bigDecimal)(BigDecimal.apply)

    def bigDecimal(using Profunctor[F]): F[BigDecimal, BigDecimal] = bigDecimal(Validation.valid)

    def jBigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger]): F[JBigInteger, JBigInteger] =
      F.bigInteger(validation)

    val jBigInteger: F[JBigInteger, JBigInteger] = jBigInteger(Validation.valid)

    def bigInt(validation: Validation[Constraint.Primitive.Number, BigInt])(using
        P: Profunctor[F]
    ): F[BigInt, BigInt] =
      P.dimap(jBigInteger(validation.contramap(BigInt.apply)))((_: BigInt).bigInteger)(BigInt.apply)

    def bigInt(using Profunctor[F]): F[BigInt, BigInt] = bigInt(Validation.valid)

    def double(validation: Validation[Constraint.Primitive.Number, Double]): F[Double, Double] = F.double(validation)

    val double: F[Double, Double] = double(Validation.valid)

    def float(validation: Validation[Constraint.Primitive.Number, Float]): F[Float, Float] = F.float(validation)

    val float: F[Float, Float] = float(Validation.valid)

    def int(validation: Validation[Constraint.Primitive.Number, Int]): F[Int, Int] = F.int(validation)

    val int: F[Int, Int] = int(Validation.valid)

    def long(validation: Validation[Constraint.Primitive.Number, Long]): F[Long, Long] = F.long(validation)

    val long: F[Long, Long] = long(Validation.valid)

  trait Text[F[-_, +_]](using F: PrimitiveOperation.Text[F]):
    def string(validation: Validation[Constraint.Primitive.Text, String]): F[String, String] = F.string(validation)

    val string: F[String, String] = string(Validation.valid)

    def codec[A](name: String, parse: String => Either[String, A], print: A => String): F[A, A] =
      F.codec(name, parse, print)

    /** A schema that can only be read. The print half is total: no value of type `Nothing` can reach it. */
    def parser[A](name: String, parse: String => Either[String, A]): F[Nothing, A] =
      F.codec(name, parse, identity[Nothing])

    /** A schema that can only be written. */
    def printer[A](name: String, print: A => String): F[A, Any] =
      F.codec(name, _ => Left(s"$name is write only"), print)
