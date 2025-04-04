package io.taig.otter

import cats.syntax.all.*
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.regex.Pattern
import scala.Boolean as SBoolean
import java.util.UUID
import java.lang.String as JString

trait PrimitiveInvariant[Self[_]] extends CodecInvariant[Self]

object PrimitiveInvariant:
  trait Boolean[Self[_]] extends PrimitiveInvariant[Self]:
    def boolean: Self[SBoolean]

  object Boolean:
    def apply[Self[_]](
        lift: [A] => (codec: Primitive.Boolean[A]) => Self[A],
        extract: [A] => (codec: Self[A]) => Primitive.Boolean[A]
    ): PrimitiveInvariant.Boolean[Self] = new PrimitiveInvariant.Boolean[Self]:
      override val boolean: Self[SBoolean] = lift(Primitive.Boolean.Root(metadata = Metadata.Empty))

      extension [A](self: Self[A])
        override def metadata: Metadata = extract(self).metadata
        override def modifyMetadata(f: Metadata => Metadata): Self[A] =
          lift(extract(self).modifyMetadata(f))
        override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))

  trait Number[Self[_]] extends PrimitiveInvariant[Self]:
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
        minimum: Option[Comparison[Double]] = none,
        maximum: Option[Comparison[Double]] = none,
        multiple: Option[Double] = none
    ): Self[Double]

    final val double: Self[Double] = double()

    def float(
        minimum: Option[Comparison[Float]] = none,
        maximum: Option[Comparison[Float]] = none,
        multiple: Option[Float] = none
    ): Self[Float]

    final val float: Self[Float] = float()

    def int(
        minimum: Option[Comparison[Int]] = none,
        maximum: Option[Comparison[Int]] = none,
        multiple: Option[Int] = none
    ): Self[Int]

    final val int: Self[Int] = int()

    def long(
        minimum: Option[Comparison[Long]] = none,
        maximum: Option[Comparison[Long]] = none,
        multiple: Option[Long] = none
    ): Self[Long]

    final val long: Self[Long] = long()

  object Number:
    def apply[Self[_]](
        lift: [A] => (codec: Primitive.Number[A]) => Self[A],
        extract: [A] => (codec: Self[A]) => Primitive.Number[A]
    ): PrimitiveInvariant.Number[Self] = new PrimitiveInvariant.Number[Self]:
      override def double(
          minimum: Option[Comparison[Double]],
          maximum: Option[Comparison[Double]],
          multiple: Option[Double]
      ): Self[Double] = lift(Primitive.Number.Double(minimum, maximum, multiple, metadata = Metadata.Empty))

      override def float(
          minimum: Option[Comparison[Float]],
          maximum: Option[Comparison[Float]],
          multiple: Option[Float]
      ): Self[Float] = lift(Primitive.Number.Float(minimum, maximum, multiple, metadata = Metadata.Empty))

      override def int(
          minimum: Option[Comparison[Int]],
          maximum: Option[Comparison[Int]],
          multiple: Option[Int]
      ): Self[Int] = lift(Primitive.Number.Int(minimum, maximum, multiple, metadata = Metadata.Empty))

      override def jBigDecimal(
          minimum: Option[Comparison[JBigDecimal]],
          maximum: Option[Comparison[JBigDecimal]],
          multiple: Option[JBigDecimal]
      ): Self[JBigDecimal] = lift(Primitive.Number.BigDecimal(minimum, maximum, multiple, metadata = Metadata.Empty))

      override def jBigInteger(
          minimum: Option[Comparison[JBigInteger]],
          maximum: Option[Comparison[JBigInteger]],
          multiple: Option[JBigInteger]
      ): Self[JBigInteger] = lift(Primitive.Number.BigInteger(minimum, maximum, multiple, metadata = Metadata.Empty))

      override def long(
          minimum: Option[Comparison[Long]],
          maximum: Option[Comparison[Long]],
          multiple: Option[Long]
      ): Self[Long] = lift(Primitive.Number.Long(minimum, maximum, multiple, metadata = Metadata.Empty))

      extension [A](self: Self[A])
        override def metadata: Metadata = extract(self).metadata
        override def modifyMetadata(f: Metadata => Metadata): Self[A] =
          lift(extract(self).modifyMetadata(f))
        override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))

  trait String[Self[_]] extends PrimitiveInvariant[Self]:
    def string(
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        matches: Option[Pattern] = none
    ): Self[JString]

    final val string: Self[JString] = string(minimum = none, maximum = none, matches = none)

    final val pattern: Self[Pattern] = string.imap(Pattern.compile)(_.pattern)

    def parser[A](
        name: JString,
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
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

  object String:
    def apply[Self[_]](
        lift: [A] => (codec: Primitive.String[A]) => Self[A],
        extract: [A] => (codec: Self[A]) => Primitive.String[A]
    ): PrimitiveInvariant.String[Self] = new PrimitiveInvariant.String[Self]:
      override def string(
          minimum: Option[Int],
          maximum: Option[Int],
          matches: Option[Pattern]
      ): Self[JString] = lift(Primitive.String.Text(minimum, maximum, matches, metadata = Metadata.Empty))

      override def parser[A](
          name: JString,
          minimum: Option[Int],
          maximum: Option[Int],
          matches: Option[Pattern]
      )(f: JString => Either[JString, A])(g: A => JString): Self[A] =
        lift(Primitive.String.Parser(name, f, g, minimum, maximum, matches, metadata = Metadata.Empty))

      extension [A](self: Self[A])
        override def metadata: Metadata = extract(self).metadata
        override def modifyMetadata(f: Metadata => Metadata): Self[A] =
          lift(extract(self).modifyMetadata(f))
        override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))
