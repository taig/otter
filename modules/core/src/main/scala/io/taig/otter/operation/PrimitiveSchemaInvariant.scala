package io.taig.otter.operation

import io.taig.otter.Comparison

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.regex.Pattern
import scala.Boolean as SBoolean

trait PrimitiveSchemaInvariant[Self[_], Primitive[_]]
    extends PrimitiveSchemaInvariant.Boolean[Self],
      PrimitiveSchemaInvariant.Number[Self],
      PrimitiveSchemaInvariant.String[Self, Primitive]:
  self =>

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
      gK: [A] => T[A] => Self[A]
  ): PrimitiveSchemaInvariant[T, Primitive] = new PrimitiveSchemaInvariant[T, Primitive]:
    override def boolean: T[SBoolean] = fK(self.boolean)
    override def imap[A, B](fa: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))
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
    override def enriched[A]: Enriched[T[A]] = self.enriched[A].imap(fK(_))(gK(_))
    override def string(minimum: Option[Int], maximum: Option[Int], matches: Option[Pattern]): T[JString] =
      fK(self.string(minimum, maximum, matches))
    override def parser[A](name: JString, decode: JString => Either[JString, A], encode: A => JString): T[A] =
      fK(self.parser(name, decode, encode))
    override def parsed[A](schema: Primitive[A]): T[A] = fK(self.parsed(schema))

object PrimitiveSchemaInvariant:
  trait Boolean[Self[_]] extends SchemaInvariant[Self]:
    self =>

    def boolean: Self[SBoolean]

    override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
        gK: [A] => T[A] => Self[A]
    ): PrimitiveSchemaInvariant.Boolean[T] = new Boolean[T]:
      override def boolean: T[SBoolean] = fK(self.boolean)
      override def enriched[A]: Enriched[T[A]] = self.enriched[A].imap(fK(_))(gK(_))
      override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

  object Boolean:
    inline def apply[Self[_]](using
        self: PrimitiveSchemaInvariant.Boolean[Self]
    ): PrimitiveSchemaInvariant.Boolean[Self] = self

  trait Number[Self[_]] extends SchemaInvariant[Self]:
    self =>

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

    override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
        gK: [A] => T[A] => Self[A]
    ): PrimitiveSchemaInvariant.Number[T] = new Number[T]:
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

      override def enriched[A]: Enriched[T[A]] = self.enriched[A].imap(fK(_))(gK(_))
      override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

  object Number:
    inline def apply[Self[_]](using
        self: PrimitiveSchemaInvariant.Number[Self]
    ): PrimitiveSchemaInvariant.Number[Self] = self

  trait String[Self[_], Primitive[_]] extends SchemaInvariant[Self]:
    self =>

    def string(minimum: Option[Int], maximum: Option[Int], matches: Option[Pattern]): Self[JString]

    def parser[A](
        name: JString,
        decode: JString => Either[JString, A],
        encode: A => JString
    ): Self[A]

    def parsed[A](schema: Primitive[A]): Self[A]

    override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
        gK: [A] => T[A] => Self[A]
    ): PrimitiveSchemaInvariant.String[T, Primitive] = new String[T, Primitive]:
      override def string(
          minimum: Option[Int],
          maximum: Option[Int],
          matches: Option[Pattern]
      ): T[JString] = fK(self.string(minimum, maximum, matches))

      override def parser[A](
          name: JString,
          decode: JString => Either[JString, A],
          encode: A => JString
      ): T[A] = fK(self.parser(name, decode, encode))

      override def parsed[A](schema: Primitive[A]): T[A] = fK(self.parsed(schema))

      override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

      override def enriched[A]: Enriched[T[A]] = self.enriched[A].imap(fK(_))(gK(_))

  object String:
    inline def apply[Self[_], Primitive[_]](using
        self: PrimitiveSchemaInvariant.String[Self, Primitive]
    ): PrimitiveSchemaInvariant.String[Self, Primitive] = self

  inline def apply[Self[_], Primitive[_]](using
      self: PrimitiveSchemaInvariant[Self, Primitive]
  ): PrimitiveSchemaInvariant[Self, Primitive] = self
