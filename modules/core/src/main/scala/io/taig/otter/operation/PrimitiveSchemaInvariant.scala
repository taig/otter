package io.taig.otter.operation
import io.taig.validation.Constraint
import io.taig.validation.Validation

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean

trait PrimitiveSchemaInvariant[Self[_], -Value[_]]
    extends PrimitiveSchemaInvariant.Boolean[Self],
      PrimitiveSchemaInvariant.Number[Self],
      PrimitiveSchemaInvariant.String[Self, Value]:
  self =>

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
      gK: [A] => T[A] => Self[A]
  ): PrimitiveSchemaInvariant[T, Value] = new PrimitiveSchemaInvariant[T, Value]:
    override def imap[A, B](fa: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))
    override def boolean: T[SBoolean] = fK(self.boolean)
    override def jBigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): T[JBigDecimal] =
      fK(self.jBigDecimal(validation))
    override def jBigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger]): T[JBigInteger] =
      fK(self.jBigInteger(validation))
    override def double(validation: Validation[Constraint.Primitive.Number, Double]): T[Double] =
      fK(self.double(validation))
    override def float(validation: Validation[Constraint.Primitive.Number, Float]): T[Float] =
      fK(self.float(validation))
    override def int(validation: Validation[Constraint.Primitive.Number, Int]): T[Int] =
      fK(self.int(validation))
    override def long(validation: Validation[Constraint.Primitive.Number, Long]): T[Long] =
      fK(self.long(validation))
    override def string(validation: Validation[Constraint.Primitive.Text, JString]): T[JString] =
      fK(self.string(validation))
    override def parser[A](name: JString, decode: JString => Either[JString, A], encode: A => JString): T[A] =
      fK(self.parser(name, decode, encode))
    override def enriched[A]: Enriched[T[A]] = self.enriched[A].imap(fK(_))(gK(_))
    override def parsed[A](schema: => Value[A]): T[A] = fK(self.parsed(schema))

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
        validation: Validation[Constraint.Primitive.Number, JBigDecimal]
    ): Self[JBigDecimal]

    def jBigInteger(
        validation: Validation[Constraint.Primitive.Number, JBigInteger]
    ): Self[JBigInteger]

    def double(validation: Validation[Constraint.Primitive.Number, Double]): Self[Double]

    def float(validation: Validation[Constraint.Primitive.Number, Float]): Self[Float]

    def int(validation: Validation[Constraint.Primitive.Number, Int]): Self[Int]

    def long(validation: Validation[Constraint.Primitive.Number, Long]): Self[Long]

    override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
        gK: [A] => T[A] => Self[A]
    ): PrimitiveSchemaInvariant.Number[T] = new Number[T]:
      override def jBigDecimal(
          validation: Validation[Constraint.Primitive.Number, JBigDecimal]
      ): T[JBigDecimal] = fK(self.jBigDecimal(validation))
      override def jBigInteger(
          validation: Validation[Constraint.Primitive.Number, JBigInteger]
      ): T[JBigInteger] = fK(self.jBigInteger(validation))
      override def double(
          validation: Validation[Constraint.Primitive.Number, Double]
      ): T[Double] = fK(self.double(validation))
      override def float(
          validation: Validation[Constraint.Primitive.Number, Float]
      ): T[Float] = fK(self.float(validation))
      override def int(
          validation: Validation[Constraint.Primitive.Number, Int]
      ): T[Int] = fK(self.int(validation))
      override def long(
          validation: Validation[Constraint.Primitive.Number, Long]
      ): T[Long] = fK(self.long(validation))
      override def enriched[A]: Enriched[T[A]] = self.enriched[A].imap(fK(_))(gK(_))
      override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

  object Number:
    inline def apply[Self[_]](using
        self: PrimitiveSchemaInvariant.Number[Self]
    ): PrimitiveSchemaInvariant.Number[Self] = self

  trait String[Self[_], -Value[_]] extends SchemaInvariant[Self]:
    self =>

    def string(validation: Validation[Constraint.Primitive.Text, JString]): Self[JString]

    def parser[A](
        name: JString,
        decode: JString => Either[JString, A],
        encode: A => JString
    ): Self[A]

    def parsed[A](schema: => Value[A]): Self[A]

    override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
        gK: [A] => T[A] => Self[A]
    ): PrimitiveSchemaInvariant.String[T, Value] = new PrimitiveSchemaInvariant.String[T, Value]:
      override def imap[A, B](fa: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))
      override def enriched[A]: Enriched[T[A]] = self.enriched[A].imap(fK(_))(gK(_))
      override def string(validation: Validation[Constraint.Primitive.Text, JString]): T[JString] =
        fK(self.string(validation))
      override def parser[A](name: JString, decode: JString => Either[JString, A], encode: A => JString): T[A] =
        fK(self.parser(name, decode, encode))
      override def parsed[A](schema: => Value[A]): T[A] = fK(self.parsed(schema))

  object String:
    inline def apply[Self[_], Value[_]](using
        self: PrimitiveSchemaInvariant.String[Self, Value]
    ): PrimitiveSchemaInvariant.String[Self, Value] = self

  inline def apply[Self[_], Value[_]](using
      self: PrimitiveSchemaInvariant[Self, Value]
  ): PrimitiveSchemaInvariant[Self, Value] = self
