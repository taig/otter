package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.operation.PrimitiveSchemaInvariant

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.regex.Pattern
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong

sealed abstract class Primitive[A] extends Product with Serializable:
  def mapK[S[_] >: Nothing, T[_]](fK: [A] => S[A] => T[A]): Primitive[A]
  def imap[B](f: A => B)(g: B => A): Primitive[B]

object Primitive:
  sealed abstract class Boolean[A] extends Primitive[A]:
    final override def mapK[S[_] >: Nothing, T[_]](fK: [A] => S[A] => T[A]): Primitive.Boolean[A] = this
    override def imap[B](f: A => B)(g: B => A): Primitive.Boolean[B] = Boolean.Modify(self = this, f, g)

  object Boolean:
    final private[otter] case class Modify[A, B](self: Primitive.Boolean[A], f: A => B, g: B => A)
        extends Primitive.Boolean[B]

    private[otter] case object Root extends Primitive.Boolean[SBoolean]

    given schema: PrimitiveSchemaInvariant.Boolean[Primitive.Boolean] with
      override def boolean: Boolean[SBoolean] = Root

      override def imap[A, B](fa: Primitive.Boolean[A])(f: A => B)(g: B => A): Primitive.Boolean[B] = fa.imap(f)(g)

  sealed abstract class Number[A] extends Primitive[A]:
    final override def mapK[S[_] >: Nothing, T[_]](fK: [A] => S[A] => T[A]): Primitive.Number[A] = this
    final override def imap[B](f: A => B)(g: B => A): Primitive.Number[B] = Number.Modify(self = this, f, g)

  object Number:
    final private[otter] case class BigDecimal(
        minimum: Option[Comparison[JBigDecimal]],
        maximum: Option[Comparison[JBigDecimal]],
        multiple: Option[JBigDecimal]
    ) extends Primitive.Number[JBigDecimal]

    final private[otter] case class BigInteger(
        minimum: Option[Comparison[JBigInteger]],
        maximum: Option[Comparison[JBigInteger]],
        multiple: Option[JBigInteger]
    ) extends Primitive.Number[JBigInteger]

    final private[otter] case class Double(
        minimum: Option[Comparison[SDouble]],
        maximum: Option[Comparison[SDouble]],
        multiple: Option[SDouble]
    ) extends Primitive.Number[SDouble]

    final private[otter] case class Float(
        minimum: Option[Comparison[SFloat]],
        maximum: Option[Comparison[SFloat]],
        multiple: Option[SFloat]
    ) extends Primitive.Number[SFloat]

    final private[otter] case class Int(
        minimum: Option[Comparison[SInt]],
        maximum: Option[Comparison[SInt]],
        multiple: Option[SInt]
    ) extends Primitive.Number[SInt]

    final private[otter] case class Long(
        minimum: Option[Comparison[SLong]],
        maximum: Option[Comparison[SLong]],
        multiple: Option[SLong]
    ) extends Primitive.Number[SLong]

    final private[otter] case class Modify[A, B](
        self: Primitive.Number[A],
        f: A => B,
        g: B => A
    ) extends Primitive.Number[B]

    given schema: PrimitiveSchemaInvariant.Number[Primitive.Number] with
      override def jBigDecimal(
          minimum: Option[Comparison[JBigDecimal]],
          maximum: Option[Comparison[JBigDecimal]],
          multiple: Option[JBigDecimal]
      ): Number[JBigDecimal] = BigDecimal(minimum, maximum, multiple)

      override def jBigInteger(
          minimum: Option[Comparison[JBigInteger]],
          maximum: Option[Comparison[JBigInteger]],
          multiple: Option[JBigInteger]
      ): Number[JBigInteger] = BigInteger(minimum, maximum, multiple)

      override def double(
          minimum: Option[Comparison[SDouble]],
          maximum: Option[Comparison[SDouble]],
          multiple: Option[SDouble]
      ): Number[SDouble] = Double(minimum, maximum, multiple)

      override def float(
          minimum: Option[Comparison[SFloat]],
          maximum: Option[Comparison[SFloat]],
          multiple: Option[SFloat]
      ): Number[SFloat] = Float(minimum, maximum, multiple)

      override def int(
          minimum: Option[Comparison[SInt]],
          maximum: Option[Comparison[SInt]],
          multiple: Option[SInt]
      ): Number[SInt] = Int(minimum, maximum, multiple)

      override def long(
          minimum: Option[Comparison[SLong]],
          maximum: Option[Comparison[SLong]],
          multiple: Option[SLong]
      ): Number[SLong] = Long(minimum, maximum, multiple)

      override def imap[A, B](fa: Primitive.Number[A])(f: A => B)(g: B => A): Primitive.Number[B] = fa.imap(f)(g)

  sealed abstract class String[A] extends Primitive[A]:
    final override def mapK[S[_] >: Nothing, T[_]](fK: [A] => S[A] => T[A]): Primitive.String[A] = this
    final override def imap[B](f: A => B)(g: B => A): Primitive.String[B] = String.Modify(self = this, f, g)

  object String:
    final private[otter] case class Parser[A](
        name: JString,
        decode: JString => Either[JString, A],
        encode: A => JString,
        minimum: Option[SInt],
        maximum: Option[SInt],
        matches: Option[Pattern]
    ) extends Primitive.String[A]

    final private[otter] case class Text(
        minimum: Option[SInt],
        maximum: Option[SInt],
        matches: Option[Pattern]
    ) extends Primitive.String[JString]

    final private[otter] case class Modify[A, B](
        self: Primitive.String[A],
        f: A => B,
        g: B => A
    ) extends Primitive.String[B]

    given schema: PrimitiveSchemaInvariant.String[Primitive.String] with
      override def string(
          minimum: Option[SInt],
          maximum: Option[SInt],
          matches: Option[Pattern]
      ): String[JString] = Text(minimum, maximum, matches)

      override def parser[A](
          name: JString,
          decode: JString => Either[JString, A],
          encode: A => JString,
          minimum: Option[SInt],
          maximum: Option[SInt],
          matches: Option[Pattern]
      ): String[A] = Parser(name, decode, encode, minimum, maximum, matches)

      override def imap[A, B](fa: Primitive.String[A])(f: A => B)(g: B => A): Primitive.String[B] = fa.imap(f)(g)

  given PrimitiveSchemaInvariant[Primitive] with
    export Primitive.Boolean.schema.boolean
    export Primitive.Number.schema.{double, float, int, jBigDecimal, jBigInteger, long}
    export Primitive.String.schema.{parser, string}

    override def imap[A, B](fa: Primitive[A])(f: A => B)(g: B => A): Primitive[B] = fa.imap(f)(g)
