package io.taig.otter.schema

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.lang.String as JString
import scala.Boolean as SBoolean
import io.taig.otter.Metadata
import io.taig.otter.Comparison
import java.util.regex.Pattern

trait PrimitiveSchema[Self[_]]
    extends PrimitiveSchema.Boolean[Self],
      PrimitiveSchema.Number[Self],
      PrimitiveSchema.String[Self]:
  self =>

  final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): PrimitiveSchema[T] =
    new PrimitiveSchema[T]:
      extension [A](fa: T[A])
        override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))
        override def metadata: Metadata = self.metadata(gK(fa))
        override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(fa))(f))

      override def boolean: T[Boolean] = fK(self.boolean)

      override def jBigDecimal(
          minimum: Option[Comparison[JBigDecimal]],
          maximum: Option[Comparison[JBigDecimal]],
          multiple: Option[JBigDecimal]
      ): T[JBigDecimal] = fK(self.jBigDecimal(minimum, maximum, multiple))

      override def jBigInteger(
          minimum: Option[Comparison[JBigInteger]],
          maximum: Option[Comparison[JBigInteger]],
          multiple: Option[JBigInteger]
      ): T[JBigInteger] = fK(self.jBigInteger(minimum, maximum, multiple))

      override def double(
          minimum: Option[Comparison[Double]],
          maximum: Option[Comparison[Double]],
          multiple: Option[Double]
      ): T[Double] = fK(self.double(minimum, maximum, multiple))

      override def float(
          minimum: Option[Comparison[Float]],
          maximum: Option[Comparison[Float]],
          multiple: Option[Float]
      ): T[Float] = fK(self.float(minimum, maximum, multiple))

      override def int(
          minimum: Option[Comparison[Int]],
          maximum: Option[Comparison[Int]],
          multiple: Option[Int]
      ): T[Int] = fK(self.int(minimum, maximum, multiple))

      override def long(
          minimum: Option[Comparison[Long]],
          maximum: Option[Comparison[Long]],
          multiple: Option[Long]
      ): T[Long] = fK(self.long(minimum, maximum, multiple))

      override def string(minimum: Option[Int], maximum: Option[Int], matches: Option[Pattern]): T[JString] =
        fK(self.string(minimum, maximum, matches))

      override def parser[A](
          name: JString,
          decode: JString => Either[JString, A],
          encode: A => JString,
          minimum: Option[Int],
          maximum: Option[Int],
          matches: Option[Pattern]
      ): T[A] = fK(self.parser(name, decode, encode, minimum, maximum, matches))

object PrimitiveSchema:
  trait Boolean[Self[_]] extends Schema[Self]:
    self =>

    override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
        gK: [A] => T[A] => Self[A]
    ): PrimitiveSchema.Boolean[T] = new Boolean[T]:
      override def boolean: T[SBoolean] = fK(self.boolean)

      extension [A](fa: T[A])
        override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))
        override def metadata: Metadata = self.metadata(gK(fa))
        override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(fa))(f))

    def boolean: Self[SBoolean]

  object Boolean:
    inline def apply[Self[_]](using self: PrimitiveSchema.Boolean[Self]): PrimitiveSchema.Boolean[Self] = self

  trait Number[Self[_]] extends Schema[Self]:
    self =>

    override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
        gK: [A] => T[A] => Self[A]
    ): PrimitiveSchema.Number[T] = new Number[T]:
      extension [A](ta: T[A])
        override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))
        override def metadata: Metadata = self.metadata(gK(ta))
        override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))

      override def jBigDecimal(
          minimum: Option[Comparison[JBigDecimal]],
          maximum: Option[Comparison[JBigDecimal]],
          multiple: Option[JBigDecimal]
      ): T[JBigDecimal] =
        fK(self.jBigDecimal(minimum, maximum, multiple))

      override def jBigInteger(
          minimum: Option[Comparison[JBigInteger]],
          maximum: Option[Comparison[JBigInteger]],
          multiple: Option[JBigInteger]
      ): T[JBigInteger] =
        fK(self.jBigInteger(minimum, maximum, multiple))

      override def double(
          minimum: Option[Comparison[Double]],
          maximum: Option[Comparison[Double]],
          multiple: Option[Double]
      ): T[Double] =
        fK(self.double(minimum, maximum, multiple))

      override def float(
          minimum: Option[Comparison[Float]],
          maximum: Option[Comparison[Float]],
          multiple: Option[Float]
      ): T[Float] =
        fK(self.float(minimum, maximum, multiple))

      override def int(
          minimum: Option[Comparison[Int]],
          maximum: Option[Comparison[Int]],
          multiple: Option[Int]
      ): T[Int] =
        fK(self.int(minimum, maximum, multiple))

      override def long(
          minimum: Option[Comparison[Long]],
          maximum: Option[Comparison[Long]],
          multiple: Option[Long]
      ): T[Long] =
        fK(self.long(minimum, maximum, multiple))

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
        minimum: Option[Comparison[Double]],
        maximum: Option[Comparison[Double]],
        multiple: Option[Double]
    ): Self[Double]

    def float(
        minimum: Option[Comparison[Float]],
        maximum: Option[Comparison[Float]],
        multiple: Option[Float]
    ): Self[Float]

    def int(
        minimum: Option[Comparison[Int]],
        maximum: Option[Comparison[Int]],
        multiple: Option[Int]
    ): Self[Int]

    def long(
        minimum: Option[Comparison[Long]],
        maximum: Option[Comparison[Long]],
        multiple: Option[Long]
    ): Self[Long]

  object Number:
    inline def apply[Self[_]](using self: PrimitiveSchema.Number[Self]): PrimitiveSchema.Number[Self] = self

  trait String[Self[_]] extends Schema[Self]:
    self =>

    override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
        gK: [A] => T[A] => Self[A]
    ): PrimitiveSchema.String[T] =
      new String[T]:
        override def string(
            minimum: Option[Int],
            maximum: Option[Int],
            matches: Option[Pattern]
        ): T[JString] = fK(self.string(minimum, maximum, matches))

        override def parser[A](
            name: JString,
            decode: JString => Either[JString, A],
            encode: A => JString,
            minimum: Option[Int],
            maximum: Option[Int],
            matches: Option[Pattern]
        ): T[A] =
          fK(self.parser(name, decode, encode, minimum, maximum, matches))

        extension [A](ta: T[A])
          override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))
          override def metadata: Metadata = self.metadata(gK(ta))
          override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))

    def string(minimum: Option[Int], maximum: Option[Int], matches: Option[Pattern]): Self[JString]

    def parser[A](
        name: JString,
        decode: JString => Either[JString, A],
        encode: A => JString,
        minimum: Option[Int],
        maximum: Option[Int],
        matches: Option[Pattern]
    ): Self[A]

  object String:
    inline def apply[Self[_]](using self: PrimitiveSchema.String[Self]): PrimitiveSchema.String[Self] = self

  inline def apply[Self[_]](using self: PrimitiveSchema[Self]): PrimitiveSchema[Self] = self
