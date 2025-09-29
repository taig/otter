package io.taig.otter.component

import cats.syntax.all.*
import io.taig.Undefined
import io.taig.otter.operation.PrimitiveSchemaInvariant
import io.taig.otter.validation.Comparison

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.UUID
import java.util.regex.Pattern
import scala.BigDecimal as SBigDecimal
import scala.BigInt as SBigInt
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong

trait PrimitiveComponent[+Self[_]]
    extends PrimitiveComponent.Boolean[Self],
      PrimitiveComponent.Number[Self],
      PrimitiveComponent.String[Self]

object PrimitiveComponent:
  trait Boolean[+Self[_]](using self: PrimitiveSchemaInvariant.Boolean[Self]):
    final val boolean: Self[SBoolean] = self.boolean

  trait Number[+Self[_]](using self: PrimitiveSchemaInvariant.Number[Self]):
    final def jBigDecimal(
        minimum: Undefined.Or[Comparison[JBigDecimal]] = Undefined,
        maximum: Undefined.Or[Comparison[JBigDecimal]] = Undefined,
        multiple: Undefined.Or[JBigDecimal] = Undefined
    ): Self[JBigDecimal] = self.jBigDecimal(
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      multiple = multiple.toOption
    )

    final val jBigDecimal: Self[JBigDecimal] = jBigDecimal()

    final def bigDecimal(
        minimum: Undefined.Or[Comparison[SBigDecimal]] = Undefined,
        maximum: Undefined.Or[Comparison[SBigDecimal]] = Undefined,
        multiple: Undefined.Or[SBigDecimal] = Undefined
    ): Self[SBigDecimal] = jBigDecimal(
      minimum = minimum.map(_.map(_.bigDecimal)),
      maximum = maximum.map(_.map(_.bigDecimal)),
      multiple = multiple.map(_.bigDecimal)
    ).imap(SBigDecimal.apply)(_.bigDecimal)

    final val bigDecimal: Self[SBigDecimal] = bigDecimal()

    final def jBigInteger(
        minimum: Undefined.Or[Comparison[JBigInteger]] = Undefined,
        maximum: Undefined.Or[Comparison[JBigInteger]] = Undefined,
        multiple: Undefined.Or[JBigInteger] = Undefined
    ): Self[JBigInteger] = self.jBigInteger(
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      multiple = multiple.toOption
    )

    final val jBigInteger: Self[JBigInteger] = jBigInteger()

    final def bigInteger(
        minimum: Undefined.Or[Comparison[SBigInt]] = Undefined,
        maximum: Undefined.Or[Comparison[SBigInt]] = Undefined,
        multiple: Undefined.Or[SBigInt] = Undefined
    ): Self[SBigInt] = jBigInteger(
      minimum = minimum.map(_.map(_.bigInteger)),
      maximum = maximum.map(_.map(_.bigInteger)),
      multiple = multiple.map(_.bigInteger)
    ).imap(SBigInt.apply)(_.bigInteger)

    final val bigInteger: Self[SBigInt] = bigInteger()

    final def double(
        minimum: Undefined.Or[Comparison[SDouble]] = Undefined,
        maximum: Undefined.Or[Comparison[SDouble]] = Undefined,
        multiple: Undefined.Or[SDouble] = Undefined
    ): Self[SDouble] = self.double(
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      multiple = multiple.toOption
    )

    final val double: Self[SDouble] = double()

    final def float(
        minimum: Undefined.Or[Comparison[SFloat]] = Undefined,
        maximum: Undefined.Or[Comparison[SFloat]] = Undefined,
        multiple: Undefined.Or[SFloat] = Undefined
    ): Self[SFloat] = self.float(
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      multiple = multiple.toOption
    )

    final val float: Self[SFloat] = float()

    final def int(
        minimum: Undefined.Or[Comparison[SInt]] = Undefined,
        maximum: Undefined.Or[Comparison[SInt]] = Undefined,
        multiple: Undefined.Or[SInt] = Undefined
    ): Self[SInt] = self.int(
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      multiple = multiple.toOption
    )

    final val int: Self[SInt] = int()

    final def long(
        minimum: Undefined.Or[Comparison[SLong]] = Undefined,
        maximum: Undefined.Or[Comparison[SLong]] = Undefined,
        multiple: Undefined.Or[SLong] = Undefined
    ): Self[SLong] = self.long(
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      multiple = multiple.toOption
    )

    final val long: Self[SLong] = long()

  trait String[+Self[_]](using self: PrimitiveSchemaInvariant.String[Self, ?]):
    final def string(
        minimum: Undefined.Or[SInt] = Undefined,
        maximum: Undefined.Or[SInt] = Undefined,
        matches: Undefined.Or[Pattern] = Undefined
    ): Self[JString] = self.string(
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      matches = matches.toOption
    )

    final val string: Self[JString] = string()

    implicit final class ToStringComponentExtension(dummy: string.type) extends StringComponentExtension[Self, JString]:
      override protected def empty: JString = ""
      override protected def isEmpty(a: JString): SBoolean = a.isEmpty

      def apply(
          minimum: Undefined.Or[Int] = Undefined,
          maximum: Undefined.Or[Int] = Undefined,
          matches: Undefined.Or[Pattern] = Undefined
      ): Self[JString] = string(minimum, maximum, matches)

    final def parser[A](name: JString)(f: JString => Either[JString, A])(g: A => JString): Self[A] =
      self.parser(name, decode = f, encode = g)

    final val uuid: Self[UUID] = parser(name = "uuid") { value =>
      Either.catchOnly[IllegalArgumentException](UUID.fromString(value)).leftMap(_.getMessage)
    }(_.show)

    final val pattern: Self[Pattern] = string.imap(Pattern.compile)(_.pattern)
