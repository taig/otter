package io.taig.otter

import cats.syntax.all.*
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.regex.Pattern
import cats.Invariant
import java.util.UUID
import java.lang.String as JString

abstract class PrimitiveInvariant[Self[_]] extends CodecInvariant[Self]

object PrimitiveInvariant:
  abstract class Number[Self[_]] extends PrimitiveInvariant[Self]:
    def lift[A](codec: Primitive.Number[A]): Self[A]

    final def jBigDecimal(
        minimum: Option[Comparison[JBigDecimal]] = none,
        maximum: Option[Comparison[JBigDecimal]] = none,
        multiple: Option[JBigDecimal] = none
    ): Self[JBigDecimal] = lift(Primitive.Number.BigDecimal(minimum, maximum, multiple, metadata = Metadata.Empty))

    final def jBigDecimal: Self[JBigDecimal] = jBigDecimal(minimum = none, maximum = none, multiple = none)

    final def jBigInteger(
        minimum: Option[Comparison[JBigInteger]] = none,
        maximum: Option[Comparison[JBigInteger]] = none,
        multiple: Option[JBigInteger] = none
    ): Self[JBigInteger] = ???

    final def jBigInteger: Self[JBigInteger] = jBigInteger(minimum = none, maximum = none, multiple = none)

    final def double(
        minimum: Option[Comparison[Double]] = none,
        maximum: Option[Comparison[Double]] = none,
        multiple: Option[Double] = none
    ): Self[Double] = ???

    final val double: Self[Double] = double()

    final def float(
        minimum: Option[Comparison[Float]] = none,
        maximum: Option[Comparison[Float]] = none,
        multiple: Option[Float] = none
    ): Self[Float] = ???

    final val float: Self[Float] = float()

    final def int(
        minimum: Option[Comparison[Int]] = none,
        maximum: Option[Comparison[Int]] = none,
        multiple: Option[Int] = none
    ): Self[Int] = ???

    final val int: Self[Int] = int()

    final def long(
        minimum: Option[Comparison[Long]] = none,
        maximum: Option[Comparison[Long]] = none,
        multiple: Option[Long] = none
    ): Self[Long] = ???

  abstract class String[Self[_]] extends PrimitiveInvariant[Self]:
    def lift[A](codec: Primitive.String[A]): Self[A]

    final def string(
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        matches: Option[Pattern] = none
    ): Self[JString] = lift(Primitive.String.Text(minimum, maximum, matches, metadata = Metadata.Empty))

    final def pattern(pattern: Pattern): Self[JString] = ???