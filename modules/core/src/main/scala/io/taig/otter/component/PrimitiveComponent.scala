package io.taig.otter.component

import cats.syntax.all.*
import io.taig.otter.Argument
import io.taig.otter.Comparison
import io.taig.otter.operation.PrimitiveSchemaInvariant

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

trait PrimitiveComponent[+Self[_], +String[a] <: Self[a]](using self: PrimitiveSchemaInvariant[Self, String])
    extends PrimitiveComponent.Boolean[Self],
      PrimitiveComponent.Number[Self],
      PrimitiveComponent.String[Self]

object PrimitiveComponent:
  trait Boolean[+Self[_]](using self: PrimitiveSchemaInvariant.Boolean[Self]):
    final val boolean: Self[SBoolean] = self.boolean

  trait Number[+Self[_]](using self: PrimitiveSchemaInvariant.Number[Self]):
    final def jBigDecimal(
        minimum: Argument[Comparison[JBigDecimal]] = Argument.Default,
        maximum: Argument[Comparison[JBigDecimal]] = Argument.Default,
        multiple: Argument[JBigDecimal] = Argument.Default
    ): Self[JBigDecimal] = self.jBigDecimal(
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      multiple = multiple.toOption
    )

    final val jBigDecimal: Self[JBigDecimal] = jBigDecimal()

    final def bigDecimal(
        minimum: Argument[Comparison[SBigDecimal]] = Argument.Default,
        maximum: Argument[Comparison[SBigDecimal]] = Argument.Default,
        multiple: Argument[SBigDecimal] = Argument.Default
    ): Self[SBigDecimal] = jBigDecimal(
      minimum = minimum.map(_.map(_.bigDecimal)),
      maximum = maximum.map(_.map(_.bigDecimal)),
      multiple = multiple.map(_.bigDecimal)
    ).imap(SBigDecimal.apply)(_.bigDecimal)

    final val bigDecimal: Self[SBigDecimal] = bigDecimal()

    final def jBigInteger(
        minimum: Argument[Comparison[JBigInteger]] = Argument.Default,
        maximum: Argument[Comparison[JBigInteger]] = Argument.Default,
        multiple: Argument[JBigInteger] = Argument.Default
    ): Self[JBigInteger] = self.jBigInteger(
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      multiple = multiple.toOption
    )

    final val jBigInteger: Self[JBigInteger] = jBigInteger()

    final def bigInteger(
        minimum: Argument[Comparison[SBigInt]] = Argument.Default,
        maximum: Argument[Comparison[SBigInt]] = Argument.Default,
        multiple: Argument[SBigInt] = Argument.Default
    ): Self[SBigInt] = jBigInteger(
      minimum = minimum.map(_.map(_.bigInteger)),
      maximum = maximum.map(_.map(_.bigInteger)),
      multiple = multiple.map(_.bigInteger)
    ).imap(SBigInt.apply)(_.bigInteger)

    final val bigInteger: Self[SBigInt] = bigInteger()

    final def double(
        minimum: Argument[Comparison[SDouble]] = Argument.Default,
        maximum: Argument[Comparison[SDouble]] = Argument.Default,
        multiple: Argument[SDouble] = Argument.Default
    ): Self[SDouble] = self.double(
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      multiple = multiple.toOption
    )

    final val double: Self[SDouble] = double()

    final def float(
        minimum: Argument[Comparison[SFloat]] = Argument.Default,
        maximum: Argument[Comparison[SFloat]] = Argument.Default,
        multiple: Argument[SFloat] = Argument.Default
    ): Self[SFloat] = self.float(
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      multiple = multiple.toOption
    )

    final val float: Self[SFloat] = float()

    final def int(
        minimum: Argument[Comparison[SInt]] = Argument.Default,
        maximum: Argument[Comparison[SInt]] = Argument.Default,
        multiple: Argument[SInt] = Argument.Default
    ): Self[SInt] = self.int(
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      multiple = multiple.toOption
    )

    final val int: Self[SInt] = int()

    final def long(
        minimum: Argument[Comparison[SLong]] = Argument.Default,
        maximum: Argument[Comparison[SLong]] = Argument.Default,
        multiple: Argument[SLong] = Argument.Default
    ): Self[SLong] = self.long(
      minimum = minimum.toOption,
      maximum = maximum.toOption,
      multiple = multiple.toOption
    )

    final val long: Self[SLong] = long()

  trait String[+Self[_]](using self: PrimitiveSchemaInvariant.String[Self]):
    final def string(
        minimum: Argument[SInt] = Argument.Default,
        maximum: Argument[SInt] = Argument.Default,
        matches: Argument[Pattern] = Argument.Default
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
          minimum: Argument[Int] = Argument.Default,
          maximum: Argument[Int] = Argument.Default,
          matches: Argument[Pattern] = Argument.Default
      ): Self[JString] = string(minimum, maximum, matches)

    final def parser[A](name: JString)(f: JString => Either[JString, A])(g: A => JString): Self[A] =
      self.parser(name, decode = f, encode = g)

    final val uuid: Self[UUID] = parser(name = "uuid") { value =>
      Either.catchOnly[IllegalArgumentException](UUID.fromString(value)).leftMap(_.getMessage)
    }(_.show)

    final val pattern: Self[Pattern] = string.imap(Pattern.compile)(_.pattern)
