package io.taig.otter

import cats.syntax.all.*
import cats.~>

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.regex.Pattern
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong
import java.util.UUID

sealed abstract class Primitive[A] extends Codec[Nothing, A]:
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

    trait Syntax[Self[_]] extends Codec.Syntax[Self]:
      def boolean: Self[SBoolean]

    object Syntax:
      trait Default[Self[_]] extends Primitive.Boolean.Syntax[Self]:
        def fromBoolean[A](codec: Primitive.Boolean[A]): Self[A]
        def toBoolean[A](self: Self[A]): Primitive.Boolean[A]

        final override val boolean: Self[SBoolean] = fromBoolean(Primitive.Boolean.Root(metadata = Metadata.Empty))

        extension [A](self: Self[A])
          final override def imap[B](f: A => B)(g: B => A): Self[B] = fromBoolean(toBoolean(self).imap(f)(g))
          final override def metadata: Metadata = toBoolean(self).metadata
          final override def modifyMetadata(f: Metadata => Metadata): Self[A] =
            fromBoolean(toBoolean(self).modifyMetadata(f))

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

    trait Syntax[Self[_]] extends Codec.Syntax[Self]:
      def jBigDecimal(
          minimum: Option[Comparison[JBigDecimal]] = none,
          maximum: Option[Comparison[JBigDecimal]] = none,
          multiple: Option[JBigDecimal] = none
      ): Self[JBigDecimal]

      final def jBigDecimal: Self[JBigDecimal] = jBigDecimal(minimum = none, maximum = none, multiple = none)

      def jBigInteger(
          minimum: Option[Comparison[JBigInteger]] = none,
          maximum: Option[Comparison[JBigInteger]] = none,
          multiple: Option[JBigInteger] = none
      ): Self[JBigInteger]

      final def jBigInteger: Self[JBigInteger] = jBigInteger(minimum = none, maximum = none, multiple = none)

      def double(
          minimum: Option[Comparison[SDouble]] = none,
          maximum: Option[Comparison[SDouble]] = none,
          multiple: Option[SDouble] = none
      ): Self[SDouble]

      final val double: Self[SDouble] = double()

      def float(
          minimum: Option[Comparison[SFloat]] = none,
          maximum: Option[Comparison[SFloat]] = none,
          multiple: Option[SFloat] = none
      ): Self[SFloat]

      final val float: Self[SFloat] = float()

      def int(
          minimum: Option[Comparison[SInt]] = none,
          maximum: Option[Comparison[SInt]] = none,
          multiple: Option[SInt] = none
      ): Self[SInt]

      final val int: Self[SInt] = int()

      def long(
          minimum: Option[Comparison[SLong]] = none,
          maximum: Option[Comparison[SLong]] = none,
          multiple: Option[SLong] = none
      ): Self[SLong]

      final val long: Self[SLong] = long()

    object Syntax:
      trait Default[Self[_]] extends Primitive.Number.Syntax[Self]:
        def fromNumber[A](codec: Primitive.Number[A]): Self[A]
        def toNumber[A](self: Self[A]): Primitive.Number[A]

        final override def jBigDecimal(
            minimum: Option[Comparison[JBigDecimal]],
            maximum: Option[Comparison[JBigDecimal]],
            multiple: Option[JBigDecimal]
        ): Self[JBigDecimal] = fromNumber(
          Primitive.Number.BigDecimal(minimum, maximum, multiple, metadata = Metadata.Empty)
        )

        final override def jBigInteger(
            minimum: Option[Comparison[JBigInteger]],
            maximum: Option[Comparison[JBigInteger]],
            multiple: Option[JBigInteger]
        ): Self[JBigInteger] = fromNumber(
          Primitive.Number.BigInteger(minimum, maximum, multiple, metadata = Metadata.Empty)
        )

        final override def double(
            minimum: Option[Comparison[SDouble]],
            maximum: Option[Comparison[SDouble]],
            multiple: Option[SDouble]
        ): Self[SDouble] = fromNumber(Primitive.Number.Double(minimum, maximum, multiple, metadata = Metadata.Empty))

        final override def float(
            minimum: Option[Comparison[SFloat]],
            maximum: Option[Comparison[SFloat]],
            multiple: Option[SFloat]
        ): Self[SFloat] = fromNumber(Primitive.Number.Float(minimum, maximum, multiple, metadata = Metadata.Empty))

        final override def int(
            minimum: Option[Comparison[SInt]],
            maximum: Option[Comparison[SInt]],
            multiple: Option[SInt]
        ): Self[SInt] = fromNumber(Primitive.Number.Int(minimum, maximum, multiple, metadata = Metadata.Empty))

        final override def long(
            minimum: Option[Comparison[SLong]],
            maximum: Option[Comparison[SLong]],
            multiple: Option[SLong]
        ): Self[SLong] = fromNumber(Primitive.Number.Long(minimum, maximum, multiple, metadata = Metadata.Empty))

        extension [A](self: Self[A])
          final override def imap[B](f: A => B)(g: B => A): Self[B] = fromNumber(toNumber(self).imap(f)(g))
          final override def metadata: Metadata = toNumber(self).metadata
          final override def modifyMetadata(f: Metadata => Metadata): Self[A] =
            fromNumber(toNumber(self).modifyMetadata(f))

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

    trait Syntax[Self[_]] extends Codec.Syntax[Self]:
      def string(
          minimum: Option[SInt] = none,
          maximum: Option[SInt] = none,
          matches: Option[Pattern] = none
      ): Self[JString]

      final val string: Self[JString] = string(minimum = none, maximum = none, matches = none)

      def parser[A](
          name: JString,
          minimum: Option[SInt] = none,
          maximum: Option[SInt] = none,
          matches: Option[Pattern] = none
      )(f: JString => Either[JString, A])(g: A => JString): Self[A]

      final val uuid: Self[UUID] = parser(name = "uuid") { value =>
        Either.catchOnly[IllegalArgumentException](UUID.fromString(value)).leftMap(_.getMessage)
      }(_.show)

      implicit final class ToStringCodecOperations(self: string.type)
          extends StringCodecOperations[Self, JString](using this):
        override protected def empty: JString = ""
        override protected def isEmpty(a: JString): SBoolean = a.isEmpty

        def apply(
            minimum: Option[Int] = none,
            maximum: Option[Int] = none,
            matches: Option[Pattern] = none
        ): Self[JString] = string(minimum, maximum, matches)

    object Syntax:
      trait Default[Self[_]] extends Primitive.String.Syntax[Self]:
        def fromString[A](codec: Primitive.String[A]): Self[A]
        def toString[A](self: Self[A]): Primitive.String[A]

        final override def string(
            minimum: Option[SInt],
            maximum: Option[SInt],
            matches: Option[Pattern]
        ): Self[JString] = fromString(Primitive.String.Text(minimum, maximum, matches, metadata = Metadata.Empty))

        override def parser[A](name: JString, minimum: Option[SInt], maximum: Option[SInt], matches: Option[Pattern])(
            f: JString => Either[JString, A]
        )(g: A => JString): Self[A] = fromString(
          Primitive.String.Parser(name, f, g, minimum, maximum, matches, metadata = Metadata.Empty)
        )

        extension [A](self: Self[A])
          final override def imap[B](f: A => B)(g: B => A): Self[B] = fromString(toString(self).imap(f)(g))
          final override def metadata: Metadata = toString(self).metadata
          final override def modifyMetadata(f: Metadata => Metadata): Self[A] =
            fromString(toString(self).modifyMetadata(f))

  trait Syntax[Self[_]]
      extends Codec.Syntax[Self],
        Primitive.Boolean.Syntax[Self],
        Primitive.Number.Syntax[Self],
        Primitive.String.Syntax[Self]

  object Syntax:
    trait Default[Self[_]] extends Primitive.Syntax[Self]:
      def fromPrimitive[A](codec: Primitive[A]): Self[A]
      def toPrimitive[A](self: Self[A]): Primitive[A]

      override val boolean: Self[SBoolean] = fromPrimitive(Primitive.Boolean.Root(metadata = Metadata.Empty))

      override def double(
          minimum: Option[Comparison[SDouble]],
          maximum: Option[Comparison[SDouble]],
          multiple: Option[SDouble]
      ): Self[SDouble] =
        fromPrimitive(Primitive.Number.Double(minimum, maximum, multiple, metadata = Metadata.Empty))
      override def float(
          minimum: Option[Comparison[SFloat]],
          maximum: Option[Comparison[SFloat]],
          multiple: Option[SFloat]
      ): Self[SFloat] =
        fromPrimitive(Primitive.Number.Float(minimum, maximum, multiple, metadata = Metadata.Empty))
      override def int(
          minimum: Option[Comparison[SInt]],
          maximum: Option[Comparison[SInt]],
          multiple: Option[SInt]
      ): Self[SInt] =
        fromPrimitive(Primitive.Number.Int(minimum, maximum, multiple, metadata = Metadata.Empty))
      override def long(
          minimum: Option[Comparison[SLong]],
          maximum: Option[Comparison[SLong]],
          multiple: Option[SLong]
      ): Self[SLong] =
        fromPrimitive(Primitive.Number.Long(minimum, maximum, multiple, metadata = Metadata.Empty))
      override def jBigDecimal(
          minimum: Option[Comparison[JBigDecimal]],
          maximum: Option[Comparison[JBigDecimal]],
          multiple: Option[JBigDecimal]
      ): Self[JBigDecimal] =
        fromPrimitive(Primitive.Number.BigDecimal(minimum, maximum, multiple, metadata = Metadata.Empty))
      override def jBigInteger(
          minimum: Option[Comparison[JBigInteger]],
          maximum: Option[Comparison[JBigInteger]],
          multiple: Option[JBigInteger]
      ): Self[JBigInteger] =
        fromPrimitive(Primitive.Number.BigInteger(minimum, maximum, multiple, metadata = Metadata.Empty))
      override def string(minimum: Option[SInt], maximum: Option[SInt], matches: Option[Pattern]): Self[JString] =
        fromPrimitive(Primitive.String.Text(minimum, maximum, matches, metadata = Metadata.Empty))
      override def parser[A](name: JString, minimum: Option[SInt], maximum: Option[SInt], matches: Option[Pattern])(
          f: JString => Either[JString, A]
      )(g: A => JString): Self[A] = fromPrimitive(
        Primitive.String.Parser(name, f, g, minimum, maximum, matches, metadata = Metadata.Empty)
      )

      extension [A](self: Self[A])
        override def imap[B](f: A => B)(g: B => A): Self[B] = fromPrimitive(toPrimitive(self).imap(f)(g))
        override def metadata: Metadata = toPrimitive(self).metadata
        override def modifyMetadata(f: Metadata => Metadata): Self[A] =
          fromPrimitive(toPrimitive(self).modifyMetadata(f))
