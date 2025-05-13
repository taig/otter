package io.taig.otter.schema

import cats.syntax.all.*
import cats.~>

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.regex.Pattern
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.BigDecimal as SBigDecimal
import scala.BigInt as SBigInt
import scala.Int as SInt
import scala.Long as SLong
import io.taig.otter.Metadata
import io.taig.otter.Comparison
import io.taig.otter.Argument
import io.taig.otter.StringComponentExtension
import java.util.UUID

sealed abstract class Primitive[A] extends Schema[Nothing, A]:
  override def modifyMetadata(f: Metadata => Metadata): Primitive[A]
  override def mapK[S[_] >: Nothing, T[_]](fK: S ~> T): Primitive[A]
  override def imap[B](f: A => B)(g: B => A): Primitive[B]

object Primitive:
  sealed abstract class Boolean[A] extends Primitive[A]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive.Boolean[A]
    final override def mapK[S[_] >: Nothing, T[_]](fK: S ~> T): Primitive.Boolean[A] = this
    override def imap[B](f: A => B)(g: B => A): Primitive.Boolean[B] = Boolean.Modify(self = this, f, g)

  object Boolean:
    final private[otter] case class Modify[A, B](self: Primitive.Boolean[A], f: A => B, g: B => A)
        extends Primitive.Boolean[B]:
      export self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Primitive.Boolean[B] = copy(self = self.modifyMetadata(f))

    final private[otter] case class Root(metadata: Metadata) extends Primitive.Boolean[SBoolean]:
      override def modifyMetadata(f: Metadata => Metadata): Primitive.Boolean[SBoolean] =
        copy(metadata = f(metadata))

    trait Shape[Self[_]] extends Schema.Shape[Self]:
      def boolean: Self[SBoolean]

    object Shape:
      def apply[Self[_]](
          lift: [A] => (self: Primitive.Boolean[A]) => Self[A],
          extract: [A] => (self: Self[A]) => Primitive.Boolean[A]
      ): Primitive.Boolean.Shape[Self] = new Shape[Self]:
        override val boolean: Self[SBoolean] = lift(Root(metadata = Metadata.Empty))

        extension [A](self: Self[A])
          override def metadata: Metadata = extract(self).metadata
          override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
          override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))

    trait Component[+Self[_]](using shape: Primitive.Boolean.Shape[Self]):
      final val boolean: Self[SBoolean] = shape.boolean

  sealed abstract class Number[A] extends Primitive[A]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive.Number[A]
    final override def mapK[S[_] >: Nothing, T[_]](fK: S ~> T): Primitive.Number[A] = this
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

    trait Shape[Self[_]] extends Schema.Shape[Self]:
      def jBigDecimal(
          minimum: Option[Comparison[JBigDecimal]],
          maximum: Option[Comparison[JBigDecimal]],
          multiple: Option[JBigDecimal]
      ): Self[JBigDecimal]

      def jBigInteger(
          minimum: Option[Comparison[JBigInteger]],
          maximum: Option[Comparison[JBigInteger]],
          multiple: Option[JBigInteger]
      ): Self[JBigInteger]

      def double(
          minimum: Option[Comparison[SDouble]],
          maximum: Option[Comparison[SDouble]],
          multiple: Option[SDouble]
      ): Self[SDouble]

      def float(
          minimum: Option[Comparison[SFloat]],
          maximum: Option[Comparison[SFloat]],
          multiple: Option[SFloat]
      ): Self[SFloat]

      def int(
          minimum: Option[Comparison[SInt]],
          maximum: Option[Comparison[SInt]],
          multiple: Option[SInt]
      ): Self[SInt]

      def long(
          minimum: Option[Comparison[SLong]],
          maximum: Option[Comparison[SLong]],
          multiple: Option[SLong]
      ): Self[SLong]

    object Shape:
      def apply[Self[_]](
          lift: [A] => (self: Primitive.Number[A]) => Self[A],
          extract: [A] => (self: Self[A]) => Primitive.Number[A]
      ): Primitive.Number.Shape[Self] = new Shape[Self]:
        override def jBigDecimal(
            minimum: Option[Comparison[JBigDecimal]],
            maximum: Option[Comparison[JBigDecimal]],
            multiple: Option[JBigDecimal]
        ): Self[JBigDecimal] = lift(BigDecimal(minimum, maximum, multiple, metadata = Metadata.Empty))

        override def jBigInteger(
            minimum: Option[Comparison[JBigInteger]],
            maximum: Option[Comparison[JBigInteger]],
            multiple: Option[JBigInteger]
        ): Self[JBigInteger] = lift(BigInteger(minimum, maximum, multiple, metadata = Metadata.Empty))

        override def double(
            minimum: Option[Comparison[SDouble]],
            maximum: Option[Comparison[SDouble]],
            multiple: Option[SDouble]
        ): Self[SDouble] = lift(Double(minimum, maximum, multiple, metadata = Metadata.Empty))

        override def float(
            minimum: Option[Comparison[SFloat]],
            maximum: Option[Comparison[SFloat]],
            multiple: Option[SFloat]
        ): Self[SFloat] = lift(Float(minimum, maximum, multiple, metadata = Metadata.Empty))

        override def int(
            minimum: Option[Comparison[SInt]],
            maximum: Option[Comparison[SInt]],
            multiple: Option[SInt]
        ): Self[SInt] = lift(Int(minimum, maximum, multiple, metadata = Metadata.Empty))

        override def long(
            minimum: Option[Comparison[SLong]],
            maximum: Option[Comparison[SLong]],
            multiple: Option[SLong]
        ): Self[SLong] = lift(Long(minimum, maximum, multiple, metadata = Metadata.Empty))

        extension [A](self: Self[A])
          override def metadata: Metadata = extract(self).metadata
          override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
          override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))

    trait Component[+Self[_]](using shape: Primitive.Number.Shape[Self]):
      final def jBigDecimal(
          minimum: Argument[Comparison[JBigDecimal]] = Argument.Default,
          maximum: Argument[Comparison[JBigDecimal]] = Argument.Default,
          multiple: Argument[JBigDecimal] = Argument.Default
      ): Self[JBigDecimal] = shape.jBigDecimal(
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
      ): Self[JBigInteger] = shape.jBigInteger(
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
      ): Self[SDouble] = shape.double(
        minimum = minimum.toOption,
        maximum = maximum.toOption,
        multiple = multiple.toOption
      )

      final val double: Self[SDouble] = double()

      final def float(
          minimum: Argument[Comparison[SFloat]] = Argument.Default,
          maximum: Argument[Comparison[SFloat]] = Argument.Default,
          multiple: Argument[SFloat] = Argument.Default
      ): Self[SFloat] = shape.float(
        minimum = minimum.toOption,
        maximum = maximum.toOption,
        multiple = multiple.toOption
      )

      final val float: Self[SFloat] = float()

      final def int(
          minimum: Argument[Comparison[SInt]] = Argument.Default,
          maximum: Argument[Comparison[SInt]] = Argument.Default,
          multiple: Argument[SInt] = Argument.Default
      ): Self[SInt] = shape.int(
        minimum = minimum.toOption,
        maximum = maximum.toOption,
        multiple = multiple.toOption
      )

      final val int: Self[SInt] = int()

      final def long(
          minimum: Argument[Comparison[SLong]] = Argument.Default,
          maximum: Argument[Comparison[SLong]] = Argument.Default,
          multiple: Argument[SLong] = Argument.Default
      ): Self[SLong] = shape.long(
        minimum = minimum.toOption,
        maximum = maximum.toOption,
        multiple = multiple.toOption
      )

      final val long: Self[SLong] = long()

  sealed abstract class String[A] extends Primitive[A]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive.String[A]
    final override def mapK[S[_] >: Nothing, T[_]](fK: S ~> T): Primitive.String[A] = this
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

    trait Shape[Self[_]] extends Schema.Shape[Self]:
      def string(minimum: Option[SInt], maximum: Option[SInt], matches: Option[Pattern]): Self[JString]

      def parser[A](
          name: JString,
          decode: JString => Either[JString, A],
          encode: A => JString,
          minimum: Option[SInt],
          maximum: Option[SInt],
          matches: Option[Pattern]
      ): Self[A]

    object Shape:
      def apply[Self[_]](
          lift: [A] => (self: Primitive.String[A]) => Self[A],
          extract: [A] => (self: Self[A]) => Primitive.String[A]
      ): Primitive.String.Shape[Self] = new Shape[Self]:
        override def string(
            minimum: Option[SInt],
            maximum: Option[SInt],
            matches: Option[Pattern]
        ): Self[JString] = lift(Text(minimum, maximum, matches, metadata = Metadata.Empty))

        override def parser[A](
            name: JString,
            decode: JString => Either[JString, A],
            encode: A => JString,
            minimum: Option[SInt],
            maximum: Option[SInt],
            matches: Option[Pattern]
        ): Self[A] = lift(Parser(name, decode, encode, minimum, maximum, matches, metadata = Metadata.Empty))

        extension [A](self: Self[A])
          override def metadata: Metadata = extract(self).metadata
          override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
          override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))

    trait Component[+Self[_]](using shape: Primitive.String.Shape[Self]):
      final def string(
          minimum: Argument[SInt] = Argument.Default,
          maximum: Argument[SInt] = Argument.Default,
          matches: Argument[Pattern] = Argument.Default
      ): Self[JString] = shape.string(
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
      )(f: JString => Either[JString, A])(g: A => JString): Self[A] = shape.parser(
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

  trait Shape[Self[_]] extends Primitive.Boolean.Shape[Self], Primitive.Number.Shape[Self], Primitive.String.Shape[Self]

  trait Component[+Self[_]]
      extends Primitive.Boolean.Component[Self],
        Primitive.Number.Component[Self],
        Primitive.String.Component[Self]
