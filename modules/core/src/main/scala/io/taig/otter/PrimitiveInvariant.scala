package io.taig.otter

import cats.syntax.all.*
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.regex.Pattern
import scala.Boolean as SBoolean
import java.util.UUID
import java.lang.String as JString

abstract class PrimitiveInvariant[Self[_]] extends CodecInvariant[Self]:
  def extract[A](self: Self[A]): Primitive[A]

  extension [A](self: Self[A]) final override def metadata: Metadata = extract(self).metadata

object PrimitiveInvariant:
  abstract class Boolean[Self[_]] extends PrimitiveInvariant[Self]:
    def lift[A](codec: Primitive.Boolean[A]): Self[A]
    override def extract[A](self: Self[A]): Primitive.Boolean[A]
    extension [A](self: Self[A])
      final override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
      final override def imap[B](f: A => B)(g: B => A): Self[B] =
        lift(extract(self).imap(f)(g))
    final val boolean: Self[SBoolean] = lift(Primitive.Boolean.Root(metadata = Metadata.Empty))

  abstract class Number[Self[_]] extends PrimitiveInvariant[Self]:
    def lift[A](codec: Primitive.Number[A]): Self[A]
    override def extract[A](self: Self[A]): Primitive.Number[A]
    extension [A](self: Self[A])
      final override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
      final override def imap[B](f: A => B)(g: B => A): Self[B] =
        lift(extract(self).imap(f)(g))

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
    ): Self[JBigInteger] = lift(Primitive.Number.BigInteger(minimum, maximum, multiple, metadata = Metadata.Empty))

    final def jBigInteger: Self[JBigInteger] = jBigInteger(minimum = none, maximum = none, multiple = none)

    final def double(
        minimum: Option[Comparison[Double]] = none,
        maximum: Option[Comparison[Double]] = none,
        multiple: Option[Double] = none
    ): Self[Double] = lift(Primitive.Number.Double(minimum, maximum, multiple, metadata = Metadata.Empty))

    final val double: Self[Double] = double()

    final def float(
        minimum: Option[Comparison[Float]] = none,
        maximum: Option[Comparison[Float]] = none,
        multiple: Option[Float] = none
    ): Self[Float] = lift(Primitive.Number.Float(minimum, maximum, multiple, metadata = Metadata.Empty))

    final val float: Self[Float] = float()

    final def int(
        minimum: Option[Comparison[Int]] = none,
        maximum: Option[Comparison[Int]] = none,
        multiple: Option[Int] = none
    ): Self[Int] = lift(Primitive.Number.Int(minimum, maximum, multiple, metadata = Metadata.Empty))

    final val int: Self[Int] = int()

    final def long(
        minimum: Option[Comparison[Long]] = none,
        maximum: Option[Comparison[Long]] = none,
        multiple: Option[Long] = none
    ): Self[Long] = lift(Primitive.Number.Long(minimum, maximum, multiple, metadata = Metadata.Empty))

    final val long: Self[Long] = long()

  abstract class String[Self[_]] extends PrimitiveInvariant[Self]:
    def lift[A](codec: Primitive.String[A]): Self[A]
    override def extract[A](self: Self[A]): Primitive.String[A]
    extension [A](self: Self[A])
      final override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
      final override def imap[B](f: A => B)(g: B => A): Self[B] =
        lift(extract(self).imap(f)(g))

    final def string(
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        matches: Option[Pattern] = none
    ): Self[JString] = lift(Primitive.String.Text(minimum, maximum, matches, metadata = Metadata.Empty))

    final val string: Self[JString] = string(minimum = none, maximum = none, matches = none)

    final val pattern: Self[Pattern] = string.imap(Pattern.compile)(_.pattern)

    final def parser[A](
        name: JString,
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        matches: Option[Pattern] = none
    )(f: JString => Either[JString, A])(g: A => JString): Self[A] =
      lift(Primitive.String.Parser(name, decode = f, encode = g, minimum, maximum, matches, metadata = Metadata.Empty))

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
