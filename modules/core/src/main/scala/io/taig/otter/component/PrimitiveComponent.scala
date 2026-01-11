package io.taig.otter.component

import io.taig.otter.operation.PrimitiveOperation
import scala.Boolean as SBoolean
import io.taig.validation.Validation
import io.taig.otter.Constraint
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import cats.Invariant
import cats.syntax.all.*

object PrimitiveComponent:
  trait Boolean[F[_]](using F: PrimitiveOperation.Boolean[F]):
    val boolean: F[SBoolean] = F.boolean

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

  trait Text[F[_], G[_], H[_]](using
      F: PrimitiveOperation.Text[F],
      G: PrimitiveOperation.Text.Read[G],
      H: PrimitiveOperation.Text.Write[H]
  ):
    def codec[A](name: String, parse: String => Either[String, A], print: A => String): F[A] =
      F.codec(name, parse, print)

    def parser[A](name: String, parse: String => Either[String, A]): G[A] = G.parser(name, parse)

    def printer[A](name: String, print: A => String): H[A] = H.printer(name, print)

    def string(validation: Validation[Constraint.Primitive.Text, String]): F[String] =
      F.string(validation)

    val string: F[String] = string(validation = Validation.valid)
