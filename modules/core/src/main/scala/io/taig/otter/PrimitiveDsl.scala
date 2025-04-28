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
        minimum: Argument[Comparison[JBigDecimal]] = Argument.Default,
        maximum: Argument[Comparison[JBigDecimal]] = Argument.Default,
        multiple: Argument[JBigDecimal] = Argument.Default
    ): Self[JBigDecimal] = codec.jBigDecimal(
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      multiple = multiple.toOption
    )

    final val jBigDecimal: Self[JBigDecimal] = jBigDecimal()

    final def bigDecimal(
        minimum: Argument[Comparison[BigDecimal]] = Argument.Default,
        maximum: Argument[Comparison[BigDecimal]] = Argument.Default,
        multiple: Argument[BigDecimal] = Argument.Default
    ): Self[BigDecimal] = jBigDecimal(
      minimum = minimum.map(_.map(_.bigDecimal)),
      maximum = maximum.map(_.map(_.bigDecimal)),
      multiple = multiple.map(_.bigDecimal)
    ).imap(BigDecimal.apply)(_.bigDecimal)

    final val bigDecimal: Self[BigDecimal] = bigDecimal()

    final def jBigInteger(
        minimum: Argument[Comparison[JBigInteger]] = Argument.Default,
        maximum: Argument[Comparison[JBigInteger]] = Argument.Default,
        multiple: Argument[JBigInteger] = Argument.Default
    ): Self[JBigInteger] = codec.jBigInteger(
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      multiple = multiple.toOption
    )

    final val jBigInteger: Self[JBigInteger] = jBigInteger()

    final def bigInteger(
        minimum: Argument[Comparison[BigInt]] = Argument.Default,
        maximum: Argument[Comparison[BigInt]] = Argument.Default,
        multiple: Argument[BigInt] = Argument.Default
    ): Self[BigInt] = jBigInteger(
      minimum = minimum.map(_.map(_.bigInteger)),
      maximum = maximum.map(_.map(_.bigInteger)),
      multiple = multiple.map(_.bigInteger)
    ).imap(BigInt.apply)(_.bigInteger)

    final val bigInteger: Self[BigInt] = bigInteger()

    final def double(
        minimum: Argument[Comparison[SDouble]] = Argument.Default,
        maximum: Argument[Comparison[SDouble]] = Argument.Default,
        multiple: Argument[SDouble] = Argument.Default
    ): Self[SDouble] = codec.double(
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      multiple = multiple.toOption
    )

    final val double: Self[SDouble] = double()

    final def float(
        minimum: Argument[Comparison[SFloat]] = Argument.Default,
        maximum: Argument[Comparison[SFloat]] = Argument.Default,
        multiple: Argument[SFloat] = Argument.Default
    ): Self[SFloat] = codec.float(
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      multiple = multiple.toOption
    )

    final val float: Self[SFloat] = float()

    final def int(
        minimum: Argument[Comparison[SInt]] = Argument.Default,
        maximum: Argument[Comparison[SInt]] = Argument.Default,
        multiple: Argument[SInt] = Argument.Default
    ): Self[SInt] = codec.int(
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      multiple = multiple.toOption
    )

    final val int: Self[SInt] = int()

    final def long(
        minimum: Argument[Comparison[SLong]] = Argument.Default,
        maximum: Argument[Comparison[SLong]] = Argument.Default,
        multiple: Argument[SLong] = Argument.Default
    ): Self[SLong] = codec.long(
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      multiple = multiple.toOption
    )

    final val long: Self[SLong] = long()

  trait String[+Self[_]](using codec: Codec.Primitive.String[Self]):
    final def string(
        minimum: Argument[SInt] = Argument.Default,
        maximum: Argument[SInt] = Argument.Default,
        matches: Argument[Pattern] = Argument.Default
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
          minimum: Argument[Int] = Argument.Default,
          maximum: Argument[Int] = Argument.Default,
          matches: Argument[Pattern] = Argument.Default
      ): Self[JString] = string(minimum, maximum, matches)

    final def parser[A](
        name: JString,
        minimum: Argument[SInt] = Argument.Default,
        maximum: Argument[SInt] = Argument.Default,
        matches: Argument[Pattern] = Argument.Default
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

    final val pattern: Self[Pattern] = string.imap(Pattern.compile)(_.pattern)
