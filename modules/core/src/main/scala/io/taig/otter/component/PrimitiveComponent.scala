package io.taig.otter.component

import cats.arrow.Profunctor
import io.taig.otter.Constraint
import io.taig.otter.operation.PrimitiveOperation
import io.taig.validation.Validation

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean

object PrimitiveComponent:
  trait Boolean[F[-_, +_], F1[A] >: F[A, A] <: F[A, A]](using F: PrimitiveOperation.Boolean[F]):
    val boolean: F1[SBoolean] = F.boolean

  trait Number[F[-_, +_], F1[A] >: F[A, A] <: F[A, A]](using F: PrimitiveOperation.Number[F]):
    def jBigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): F1[JBigDecimal] =
      F.bigDecimal(validation)

    val jBigDecimal: F1[JBigDecimal] = jBigDecimal(Validation.valid)

    def bigDecimal(validation: Validation[Constraint.Primitive.Number, BigDecimal])(using
        P: Profunctor[F]
    ): F1[BigDecimal] =
      P.dimap(jBigDecimal(validation.contramap(BigDecimal.apply)))((_: BigDecimal).bigDecimal)(BigDecimal.apply)

    def bigDecimal(using Profunctor[F]): F1[BigDecimal] = bigDecimal(Validation.valid)

    def jBigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger]): F1[JBigInteger] =
      F.bigInteger(validation)

    val jBigInteger: F1[JBigInteger] = jBigInteger(Validation.valid)

    def bigInt(validation: Validation[Constraint.Primitive.Number, BigInt])(using P: Profunctor[F]): F1[BigInt] =
      P.dimap(jBigInteger(validation.contramap(BigInt.apply)))((_: BigInt).bigInteger)(BigInt.apply)

    def bigInt(using Profunctor[F]): F1[BigInt] = bigInt(Validation.valid)

    def double(validation: Validation[Constraint.Primitive.Number, Double]): F1[Double] = F.double(validation)

    val double: F1[Double] = double(Validation.valid)

    def float(validation: Validation[Constraint.Primitive.Number, Float]): F1[Float] = F.float(validation)

    val float: F1[Float] = float(Validation.valid)

    def int(validation: Validation[Constraint.Primitive.Number, Int]): F1[Int] = F.int(validation)

    val int: F1[Int] = int(Validation.valid)

    def long(validation: Validation[Constraint.Primitive.Number, Long]): F1[Long] = F.long(validation)

    val long: F1[Long] = long(Validation.valid)

  /** @tparam FR
    *   the one parameter alias for the read direction, `F[Nothing, A]`, so [[parser]] infers as `Json.Text.Reader[A]`
    * @tparam FW
    *   the one parameter alias for the write direction, `F[A, Any]`, so [[printer]] infers as `Json.Text.Writer[A]`
    */
  trait Text[
      F[-_, +_],
      F1[A] >: F[A, A] <: F[A, A],
      FR[+A] >: F[Nothing, A] <: F[Nothing, A],
      FW[-A] >: F[A, Any] <: F[A, Any]
  ](using F: PrimitiveOperation.Text[F]):
    def string(validation: Validation[Constraint.Primitive.Text, String]): F1[String] = F.string(validation)

    val string: F1[String] = string(Validation.valid)

    def codec[A](name: String, parse: String => Either[String, A], print: A => String): F1[A] =
      F.codec(name, parse, print)

    /** A schema that can only be read. The print half is total: no value of type `Nothing` can reach it. */
    def parser[A](name: String, parse: String => Either[String, A]): FR[A] =
      F.codec(name, parse, identity[Nothing])

    /** A schema that can only be written. */
    def printer[A](name: String, print: A => String): FW[A] =
      F.codec(name, _ => Left(s"$name is write only"), print)
