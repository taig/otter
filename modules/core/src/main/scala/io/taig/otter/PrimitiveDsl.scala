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
        minimum: Value[Comparison[JBigDecimal]] = Value.Default,
        maximum: Value[Comparison[JBigDecimal]] = Value.Default,
        multiple: Value[JBigDecimal] = Value.Default
    ): Self[JBigDecimal] = codec.jBigDecimal(
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      multiple = multiple.toOption
    )

    final val jBigDecimal: Self[JBigDecimal] = jBigDecimal()

    final def bigDecimal(
        minimum: Value[Comparison[BigDecimal]] = Value.Default,
        maximum: Value[Comparison[BigDecimal]] = Value.Default,
        multiple: Value[BigDecimal] = Value.Default
    ): Self[BigDecimal] = jBigDecimal(
      minimum = minimum.map(_.map(_.bigDecimal)),
      maximum = maximum.map(_.map(_.bigDecimal)),
      multiple = multiple.map(_.bigDecimal)
    ).imap(BigDecimal.apply)(_.bigDecimal)

    final val bigDecimal: Self[BigDecimal] = bigDecimal()

    final def jBigInteger(
        minimum: Value[Comparison[JBigInteger]] = Value.Default,
        maximum: Value[Comparison[JBigInteger]] = Value.Default,
        multiple: Value[JBigInteger] = Value.Default
    ): Self[JBigInteger] = codec.jBigInteger(
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      multiple = multiple.toOption
    )

    final val jBigInteger: Self[JBigInteger] = jBigInteger()

    final def bigInteger(
        minimum: Value[Comparison[BigInt]] = Value.Default,
        maximum: Value[Comparison[BigInt]] = Value.Default,
        multiple: Value[BigInt] = Value.Default
    ): Self[BigInt] = jBigInteger(
      minimum = minimum.map(_.map(_.bigInteger)),
      maximum = maximum.map(_.map(_.bigInteger)),
      multiple = multiple.map(_.bigInteger)
    ).imap(BigInt.apply)(_.bigInteger)

    final val bigInteger: Self[BigInt] = bigInteger()

    final def double(
        minimum: Value[Comparison[SDouble]] = Value.Default,
        maximum: Value[Comparison[SDouble]] = Value.Default,
        multiple: Value[SDouble] = Value.Default
    ): Self[SDouble] = codec.double(
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      multiple = multiple.toOption
    )

    final val double: Self[SDouble] = double()

    final def float(
        minimum: Value[Comparison[SFloat]] = Value.Default,
        maximum: Value[Comparison[SFloat]] = Value.Default,
        multiple: Value[SFloat] = Value.Default
    ): Self[SFloat] = codec.float(
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      multiple = multiple.toOption
    )

    final val float: Self[SFloat] = float()

    final def int(
        minimum: Value[Comparison[SInt]] = Value.Default,
        maximum: Value[Comparison[SInt]] = Value.Default,
        multiple: Value[SInt] = Value.Default
    ): Self[SInt] = codec.int(
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      multiple = multiple.toOption
    )

    final val int: Self[SInt] = int()

    final def long(
        minimum: Value[Comparison[SLong]] = Value.Default,
        maximum: Value[Comparison[SLong]] = Value.Default,
        multiple: Value[SLong] = Value.Default
    ): Self[SLong] = codec.long(
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      multiple = multiple.toOption
    )

    final val long: Self[SLong] = long()

  trait String[+Self[_]](using codec: Codec.Primitive.String[Self]):
    final def string(
        minimum: Value[SInt] = Value.Default,
        maximum: Value[SInt] = Value.Default,
        matches: Value[Pattern] = Value.Default
    ): Self[JString] = codec.string(
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      matches = matches.toOption
    )

    final val string: Self[JString] = string()

    implicit final class ToStringCodecOperations(self: string.type)
        extends StringCodecOperations[Self, JString](using codec):
      override protected def empty: JString = ""
      override protected def isEmpty(a: JString): SBoolean = a.isEmpty

      def apply(
          minimum: Value[Int] = Value.Default,
          maximum: Value[Int] = Value.Default,
          matches: Value[Pattern] = Value.Default
      ): Self[JString] = string(minimum, maximum, matches)

    final def parser[A](
        name: JString,
        minimum: Value[SInt] = Value.Default,
        maximum: Value[SInt] = Value.Default,
        matches: Value[Pattern] = Value.Default
    )(f: JString => Either[JString, A])(g: A => JString): Self[A] = codec.parser(
      name,
      decode = f,
      encode = g,
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      matches = matches.toOption
    )

    final val uuid: Self[UUID] = parser(name = "uuid") { value =>
      Either.catchOnly[IllegalArgumentException](UUID.fromString(value)).leftMap(_.getMessage)
    }(_.show)
