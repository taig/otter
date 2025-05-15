package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Argument
import io.taig.otter.Metadata

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

sealed abstract class Primitive[A] extends Product with Serializable:
  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Primitive[A]
  def mapK[S[_] >: Nothing, T[_]](fK: [A] => S[A] => T[A]): Primitive[A]
  def imap[B](f: A => B)(g: B => A): Primitive[B]

object Primitive:
  sealed abstract class Boolean[A] extends Primitive[A]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive.Boolean[A]
    final override def mapK[S[_] >: Nothing, T[_]](fK: [A] => S[A] => T[A]): Primitive.Boolean[A] = this
    override def imap[B](f: A => B)(g: B => A): Primitive.Boolean[B] = Boolean.Modify(self = this, f, g)

  object Boolean:
    final private[otter] case class Modify[A, B](self: Primitive.Boolean[A], f: A => B, g: B => A)
        extends Primitive.Boolean[B]:
      export self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Primitive.Boolean[B] = copy(self = self.modifyMetadata(f))

    final private[otter] case class Root(metadata: Metadata) extends Primitive.Boolean[SBoolean]:
      override def modifyMetadata(f: Metadata => Metadata): Primitive.Boolean[SBoolean] =
        copy(metadata = f(metadata))

    given schema: Schema.Primitive.Boolean[Primitive.Boolean] with
      override def boolean: Boolean[SBoolean] = Root(metadata = Metadata.Empty)

      extension [A](fa: Boolean[A])
        override def imap[B](f: A => B)(g: B => A): Boolean[B] = fa.imap(f)(g)
        override def metadata: Metadata = fa.metadata
        override def modifyMetadata(f: Metadata => Metadata): Boolean[A] = fa.modifyMetadata(f)

  sealed abstract class Number[A] extends Primitive[A]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive.Number[A]
    final override def mapK[S[_] >: Nothing, T[_]](fK: [A] => S[A] => T[A]): Primitive.Number[A] = this
    final override def imap[B](f: A => B)(g: B => A): Primitive.Number[B] = Number.Modify(self = this, f, g)

  object Number:
    final private[otter] case class BigDecimal(
        minimum: Option[Comparison[JBigDecimal]],
        maximum: Option[Comparison[JBigDecimal]],
        multiple: Option[JBigDecimal],
        metadata: Metadata
    ) extends Primitive.Number[JBigDecimal]:
      override def modifyMetadata(f: Metadata => Metadata): Primitive.Number[JBigDecimal] =
        copy(metadata = f(metadata))

    final private[otter] case class BigInteger(
        minimum: Option[Comparison[JBigInteger]],
        maximum: Option[Comparison[JBigInteger]],
        multiple: Option[JBigInteger],
        metadata: Metadata
    ) extends Primitive.Number[JBigInteger]:
      override def modifyMetadata(f: Metadata => Metadata): Primitive.Number[JBigInteger] =
        copy(metadata = f(metadata))

    final private[otter] case class Double(
        minimum: Option[Comparison[SDouble]],
        maximum: Option[Comparison[SDouble]],
        multiple: Option[SDouble],
        metadata: Metadata
    ) extends Primitive.Number[SDouble]:
      override def modifyMetadata(f: Metadata => Metadata): Primitive.Number[SDouble] =
        copy(metadata = f(metadata))

    final private[otter] case class Float(
        minimum: Option[Comparison[SFloat]],
        maximum: Option[Comparison[SFloat]],
        multiple: Option[SFloat],
        metadata: Metadata
    ) extends Primitive.Number[SFloat]:
      override def modifyMetadata(f: Metadata => Metadata): Primitive.Number[SFloat] =
        copy(metadata = f(metadata))

    final private[otter] case class Int(
        minimum: Option[Comparison[SInt]],
        maximum: Option[Comparison[SInt]],
        multiple: Option[SInt],
        metadata: Metadata
    ) extends Primitive.Number[SInt]:
      override def modifyMetadata(f: Metadata => Metadata): Primitive.Number[SInt] =
        copy(metadata = f(metadata))

    final private[otter] case class Long(
        minimum: Option[Comparison[SLong]],
        maximum: Option[Comparison[SLong]],
        multiple: Option[SLong],
        metadata: Metadata
    ) extends Primitive.Number[SLong]:
      override def modifyMetadata(f: Metadata => Metadata): Primitive.Number[SLong] =
        copy(metadata = f(metadata))

    final private[otter] case class Modify[A, B](
        self: Primitive.Number[A],
        f: A => B,
        g: B => A
    ) extends Primitive.Number[B]:
      export self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Primitive.Number[B] = copy(self = self.modifyMetadata(f))

    given schema: Schema.Primitive.Number[Primitive.Number] with
      override def jBigDecimal(
          minimum: Option[Comparison[JBigDecimal]],
          maximum: Option[Comparison[JBigDecimal]],
          multiple: Option[JBigDecimal]
      ): Number[JBigDecimal] = BigDecimal(minimum, maximum, multiple, metadata = Metadata.Empty)

      override def jBigInteger(
          minimum: Option[Comparison[JBigInteger]],
          maximum: Option[Comparison[JBigInteger]],
          multiple: Option[JBigInteger]
      ): Number[JBigInteger] = BigInteger(minimum, maximum, multiple, metadata = Metadata.Empty)

      override def double(
          minimum: Option[Comparison[SDouble]],
          maximum: Option[Comparison[SDouble]],
          multiple: Option[SDouble]
      ): Number[SDouble] = Double(minimum, maximum, multiple, metadata = Metadata.Empty)

      override def float(
          minimum: Option[Comparison[SFloat]],
          maximum: Option[Comparison[SFloat]],
          multiple: Option[SFloat]
      ): Number[SFloat] = Float(minimum, maximum, multiple, metadata = Metadata.Empty)

      override def int(
          minimum: Option[Comparison[SInt]],
          maximum: Option[Comparison[SInt]],
          multiple: Option[SInt]
      ): Number[SInt] = Int(minimum, maximum, multiple, metadata = Metadata.Empty)

      override def long(
          minimum: Option[Comparison[SLong]],
          maximum: Option[Comparison[SLong]],
          multiple: Option[SLong]
      ): Number[SLong] = Long(minimum, maximum, multiple, metadata = Metadata.Empty)

      extension [A](self: Number[A])
        override def imap[B](f: A => B)(g: B => A): Number[B] = self.imap(f)(g)
        override def metadata: Metadata = self.metadata
        override def modifyMetadata(f: Metadata => Metadata): Number[A] = self.modifyMetadata(f)

  sealed abstract class String[A] extends Primitive[A]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive.String[A]
    final override def mapK[S[_] >: Nothing, T[_]](fK: [A] => S[A] => T[A]): Primitive.String[A] = this
    final override def imap[B](f: A => B)(g: B => A): Primitive.String[B] = String.Modify(self = this, f, g)

  object String:
    final private[otter] case class Parser[A](
        name: JString,
        decode: JString => Either[JString, A],
        encode: A => JString,
        minimum: Option[SInt],
        maximum: Option[SInt],
        matches: Option[Pattern],
        metadata: Metadata
    ) extends Primitive.String[A]:
      override def modifyMetadata(f: Metadata => Metadata): Primitive.String[A] = copy(metadata = f(metadata))

    final private[otter] case class Text(
        minimum: Option[SInt],
        maximum: Option[SInt],
        matches: Option[Pattern],
        metadata: Metadata
    ) extends Primitive.String[JString]:
      override def modifyMetadata(f: Metadata => Metadata): Primitive.String[JString] = copy(metadata = f(metadata))

    final private[otter] case class Modify[A, B](
        self: Primitive.String[A],
        f: A => B,
        g: B => A
    ) extends Primitive.String[B]:
      export self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Primitive.String[B] = copy(self = self.modifyMetadata(f))

    given schema: Schema.Primitive.String[Primitive.String] with
      override def string(
          minimum: Option[SInt],
          maximum: Option[SInt],
          matches: Option[Pattern]
      ): String[JString] = Text(minimum, maximum, matches, metadata = Metadata.Empty)

      override def parser[A](
          name: JString,
          decode: JString => Either[JString, A],
          encode: A => JString,
          minimum: Option[SInt],
          maximum: Option[SInt],
          matches: Option[Pattern]
      ): String[A] = Parser(name, decode, encode, minimum, maximum, matches, metadata = Metadata.Empty)

      extension [A](self: String[A])
        override def imap[B](f: A => B)(g: B => A): String[B] = self.imap(f)(g)
        override def metadata: Metadata = self.metadata
        override def modifyMetadata(f: Metadata => Metadata): String[A] = self.modifyMetadata(f)

  trait Component[+Self[_]]
      extends Primitive.Component.Boolean[Self],
        Primitive.Component.Number[Self],
        Primitive.Component.String[Self]

  object Component:
    trait Boolean[+Self[_]](using self: Schema.Primitive.Boolean[Self]):
      final val boolean: Self[SBoolean] = self.boolean

    trait Number[+Self[_]](using self: Schema.Primitive.Number[Self]):
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

    trait String[+Self[_]](using self: Schema.Primitive.String[Self]):
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

      implicit final class ToStringComponentExtension(self: string.type)
          extends StringComponentExtension[Self, JString]:
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
      )(f: JString => Either[JString, A])(g: A => JString): Self[A] = self.parser(
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

  given Schema.Primitive[Primitive] = new Schema.Primitive[Primitive]:
    extension [A](self: Primitive[A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Primitive[A] = self.modifyMetadata(f)
      override def imap[B](f: A => B)(g: B => A): Primitive[B] = self.imap(f)(g)

    export Primitive.Boolean.schema.boolean
    export Primitive.Number.schema.{double, float, int, jBigDecimal, jBigInteger, long}
    export Primitive.String.schema.{parser, string}
