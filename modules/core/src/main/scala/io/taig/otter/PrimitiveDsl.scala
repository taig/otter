package io.taig.otter

import cats.syntax.all.*

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.UUID
import java.util.regex.Pattern
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong

trait PrimitiveDsl[+Self[_]] extends PrimitiveDsl.Boolean[Self], PrimitiveDsl.Number[Self], PrimitiveDsl.String[Self]

object PrimitiveDsl:
  trait Boolean[+Self[_]](using codec: Codec.Primitive.Boolean[Self]):
    final val boolean: Self[SBoolean] = codec.boolean

  trait Number[+Self[_]](using codec: Codec.Primitive.Number[Self]):
    final def jBigDecimal(
        minimum: Option[Comparison[JBigDecimal]] = none,
        maximum: Option[Comparison[JBigDecimal]] = none,
        multiple: Option[JBigDecimal] = none
    ): Self[JBigDecimal] = codec.jBigDecimal(minimum, maximum, multiple)

    final val jBigDecimal: Self[JBigDecimal] = jBigDecimal()

    final def bigDecimal(
        minimum: Option[Comparison[BigDecimal]] = none,
        maximum: Option[Comparison[BigDecimal]] = none,
        multiple: Option[BigDecimal] = none
    ): Self[BigDecimal] = jBigDecimal(
      minimum = minimum.map(_.map(_.bigDecimal)),
      maximum = maximum.map(_.map(_.bigDecimal)),
      multiple = multiple.map(_.bigDecimal)
    ).imap(BigDecimal.apply)(_.bigDecimal)

    final val bigDecimal: Self[BigDecimal] = bigDecimal()

    final def jBigInteger(
        minimum: Option[Comparison[JBigInteger]] = none,
        maximum: Option[Comparison[JBigInteger]] = none,
        multiple: Option[JBigInteger] = none
    ): Self[JBigInteger] = codec.jBigInteger(minimum, maximum, multiple)

    final val jBigInteger: Self[JBigInteger] = jBigInteger()

    final def bigInteger(
        minimum: Option[Comparison[BigInt]] = none,
        maximum: Option[Comparison[BigInt]] = none,
        multiple: Option[BigInt] = none
    ): Self[BigInt] = jBigInteger(
      minimum = minimum.map(_.map(_.bigInteger)),
      maximum = maximum.map(_.map(_.bigInteger)),
      multiple = multiple.map(_.bigInteger)
    ).imap(BigInt.apply)(_.bigInteger)

    final val bigInteger: Self[BigInt] = bigInteger()

    final def double(
        minimum: Option[Comparison[SDouble]] = none,
        maximum: Option[Comparison[SDouble]] = none,
        multiple: Option[SDouble] = none
    ): Self[SDouble] = codec.double(minimum, maximum, multiple)

    final val double: Self[SDouble] = double()

    final def float(
        minimum: Option[Comparison[SFloat]] = none,
        maximum: Option[Comparison[SFloat]] = none,
        multiple: Option[SFloat] = none
    ): Self[SFloat] = codec.float(minimum, maximum, multiple)

    final val float: Self[SFloat] = float()

    final def int(
        minimum: Option[Comparison[SInt]] = none,
        maximum: Option[Comparison[SInt]] = none,
        multiple: Option[SInt] = none
    ): Self[SInt] = codec.int(minimum, maximum, multiple)

    final val int: Self[SInt] = int()

    final def long(
        minimum: Option[Comparison[SLong]] = none,
        maximum: Option[Comparison[SLong]] = none,
        multiple: Option[SLong] = none
    ): Self[SLong] = codec.long(minimum, maximum, multiple)

    final val long: Self[SLong] = long()

  trait String[+Self[_]](using codec: Codec.Primitive.String[Self]):
    final def string(
        minimum: Option[SInt] = none,
        maximum: Option[SInt] = none,
        matches: Option[Pattern] = none
    ): Self[JString] = codec.string(minimum, maximum, matches)

    final val string: Self[JString] = string()

    implicit final class ToStringCodecOperations(self: string.type)
        extends StringCodecOperations[Self, JString](using codec):
      override protected def empty: JString = ""
      override protected def isEmpty(a: JString): SBoolean = a.isEmpty

      def apply(
          minimum: Option[Int] = none,
          maximum: Option[Int] = none,
          matches: Option[Pattern] = none
      ): Self[JString] = string(minimum, maximum, matches)

    final def parser[A](
        name: JString,
        minimum: Option[SInt] = none,
        maximum: Option[SInt] = none,
        matches: Option[Pattern] = none
    )(f: JString => Either[JString, A])(g: A => JString): Self[A] =
      codec.parser(name, decode = f, encode = g, minimum, maximum, matches)

    final val uuid: Self[UUID] = parser(name = "uuid") { value =>
      Either.catchOnly[IllegalArgumentException](UUID.fromString(value)).leftMap(_.getMessage)
    }(_.show)
