package io.taig.otter

import cats.implicits.*
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.regex.Pattern
import cats.Invariant
import java.util.UUID

trait Primitives[S[_]: Invariant] extends Primitives.Numbers[S], Primitives.Strings[S], Primitives.Booleans[S]

object Primitives:
  trait Numbers[S[_]: Invariant]:
    def jBigDecimal(
        minimum: Option[Comparison[JBigDecimal]] = none,
        maximum: Option[Comparison[JBigDecimal]] = none,
        multiple: Option[JBigDecimal] = none
    ): S[JBigDecimal]

    final def jBigDecimal: S[JBigDecimal] = jBigDecimal(minimum = none, maximum = none, multiple = none)

    def jBigInteger(
        minimum: Option[Comparison[JBigInteger]] = none,
        maximum: Option[Comparison[JBigInteger]] = none,
        multiple: Option[JBigInteger] = none
    ): S[JBigInteger]

    final def jBigInteger: S[JBigInteger] = jBigInteger(minimum = none, maximum = none, multiple = none)

    def double(
        minimum: Option[Comparison[Double]] = none,
        maximum: Option[Comparison[Double]] = none,
        multiple: Option[Double] = none
    ): S[Double]

    final val double: S[Double] = double()

    def float(
        minimum: Option[Comparison[Float]] = none,
        maximum: Option[Comparison[Float]] = none,
        multiple: Option[Float] = none
    ): S[Float]

    final val float: S[Float] = float()

    def int(
        minimum: Option[Comparison[Int]] = none,
        maximum: Option[Comparison[Int]] = none,
        multiple: Option[Int] = none
    ): S[Int]

    final val int: S[Int] = int()

    def long(
        minimum: Option[Comparison[Long]] = none,
        maximum: Option[Comparison[Long]] = none,
        multiple: Option[Long] = none
    ): S[Long]

    val long: S[Long] = long()

  object Numbers:
    trait Defaults[S[_]] extends Primitives.Numbers[S]:
      protected def lift[A](codec: Primitive.Number[A]): S[A]

      final override def jBigDecimal(
          minimum: Option[Comparison[JBigDecimal]],
          maximum: Option[Comparison[JBigDecimal]],
          multiple: Option[JBigDecimal]
      ): S[JBigDecimal] = lift(Primitive.Number.BigDecimal(minimum, maximum, multiple, metadata = Metadata.Empty))

      final override def jBigInteger(
          minimum: Option[Comparison[JBigInteger]],
          maximum: Option[Comparison[JBigInteger]],
          multiple: Option[JBigInteger]
      ): S[JBigInteger] = lift(Primitive.Number.BigInteger(minimum, maximum, multiple, metadata = Metadata.Empty))

      final override def double(
          minimum: Option[Comparison[Double]],
          maximum: Option[Comparison[Double]],
          multiple: Option[Double]
      ): S[Double] =
        lift(Primitive.Number.Double(minimum, maximum, multiple, metadata = Metadata.Empty))

      final override def float(
          minimum: Option[Comparison[Float]],
          maximum: Option[Comparison[Float]],
          multiple: Option[Float]
      ): S[Float] =
        lift(Primitive.Number.Float(minimum, maximum, multiple, metadata = Metadata.Empty))

      final override def int(
          minimum: Option[Comparison[Int]],
          maximum: Option[Comparison[Int]],
          multiple: Option[Int]
      ): S[Int] =
        lift(Primitive.Number.Int(minimum, maximum, multiple, metadata = Metadata.Empty))

      final override def long(
          minimum: Option[Comparison[Long]],
          maximum: Option[Comparison[Long]],
          multiple: Option[Long]
      ): S[Long] =
        lift(Primitive.Number.Long(minimum, maximum, multiple, metadata = Metadata.Empty))

  trait Strings[S[_]: Invariant]:
    def string(
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        matches: Option[Pattern] = none
    ): S[String]

    final val string: S[String] = string(minimum = none, maximum = none, matches = none)

    implicit class ToStringCodecOperations(self: string.type) extends StringCodecOperations[S, String]:
      override protected def empty: String = ""
      override protected def isEmpty(a: String): Boolean = a.isEmpty

      def apply(
          minimum: Option[Int] = none,
          maximum: Option[Int] = none,
          matches: Option[Pattern] = none
      ): S[String] = string(minimum, maximum, matches)

    final val pattern: S[Pattern] = string.imap(Pattern.compile)(_.pattern)

    def parser[A](
        name: String,
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        matches: Option[Pattern] = none
    )(f: String => Either[String, A])(g: A => String): S[A]

    val uuid: S[UUID] = parser(name = "uuid") { value =>
      Either.catchOnly[IllegalArgumentException](UUID.fromString(value)).leftMap(_.getMessage)
    }(_.show)

  object Strings:
    trait Defaults[S[_]] extends Primitives.Strings[S]:
      protected def lift[A](codec: Primitive.String[A]): S[A]

      override def string(
          minimum: Option[Int],
          maximum: Option[Int],
          matches: Option[Pattern]
      ): S[String] = lift(Primitive.String.Text(minimum, maximum, matches, metadata = Metadata.Empty))

      override def parser[A](
          name: String,
          minimum: Option[Int],
          maximum: Option[Int],
          matches: Option[Pattern]
      )(f: String => Either[String, A])(g: A => String): S[A] =
        lift(
          Primitive.String.Parser(name, decode = f, encode = g, minimum, maximum, matches, metadata = Metadata.Empty)
        )

    object Plain extends Primitives.Strings.Defaults[Primitive.String]:
      final override protected inline def lift[A](codec: Primitive.String[A]): Primitive.String[A] = codec

  trait Booleans[S[_]: Invariant]:
    def boolean: S[Boolean]

  object Booleans:
    trait Defaults[S[_]] extends Primitives.Booleans[S]:
      protected def lift[A](codec: Primitive.Boolean[A]): S[A]

      override def boolean: S[Boolean] = lift(Primitive.Boolean.Root(Metadata.Empty))

  object Plain
      extends Primitives.Booleans.Defaults[Primitive.Boolean],
        Primitives.Numbers.Defaults[Primitive.Number],
        Primitives.Strings.Defaults[Primitive.String]:
    override protected inline def lift[A](codec: Primitive.Number[A]): Primitive.Number[A] = codec
    override protected inline def lift[A](codec: Primitive.Boolean[A]): Primitive.Boolean[A] = codec
    override protected inline def lift[A](codec: Primitive.String[A]): Primitive.String[A] = codec
