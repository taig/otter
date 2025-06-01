package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.operation.Enriched
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

sealed abstract class Primitive[A]:
  def value: Primitive.Value[A]
  def metadata: Metadata

object Primitive:
  final case class Boolean[A](value: Primitive.Value.Boolean[A], metadata: Metadata) extends Primitive[A]

  object Boolean:
    given PrimitiveSchemaInvariant.Boolean[Primitive.Boolean] with
      override val boolean: Primitive.Boolean[SBoolean] =
        Boolean(value = Value.Boolean.Root, metadata = Metadata.Empty)
      override def imap[A, B](fa: Primitive.Boolean[A])(f: A => B)(g: B => A): Primitive.Boolean[B] =
        fa.copy(value = fa.value.imap(f)(g))

      override def enriched[A]: Enriched[Primitive.Boolean[A]] = new Enriched[Primitive.Boolean[A]]:
        override def metadata(a: Boolean[A]): Metadata = a.metadata
        override def modifyMetadata(a: Boolean[A])(f: Metadata => Metadata): Boolean[A] =
          a.copy(metadata = f(a.metadata))

  final case class Number[A](value: Primitive.Value.Number[A], metadata: Metadata) extends Primitive[A]

  object Number:
    given PrimitiveSchemaInvariant.Number[Primitive.Number] with
      override def jBigDecimal(
          minimum: Option[Comparison[JBigDecimal]],
          maximum: Option[Comparison[JBigDecimal]],
          multiple: Option[JBigDecimal]
      ): Primitive.Number[JBigDecimal] =
        Number(value = Value.Number.BigDecimal(minimum, maximum, multiple), metadata = Metadata.Empty)

      override def jBigInteger(
          minimum: Option[Comparison[JBigInteger]],
          maximum: Option[Comparison[JBigInteger]],
          multiple: Option[JBigInteger]
      ): Primitive.Number[JBigInteger] =
        Number(value = Value.Number.BigInteger(minimum, maximum, multiple), metadata = Metadata.Empty)

      override def double(
          minimum: Option[Comparison[SDouble]],
          maximum: Option[Comparison[SDouble]],
          multiple: Option[SDouble]
      ): Primitive.Number[SDouble] =
        Number(value = Value.Number.Double(minimum, maximum, multiple), metadata = Metadata.Empty)

      override def float(
          minimum: Option[Comparison[SFloat]],
          maximum: Option[Comparison[SFloat]],
          multiple: Option[SFloat]
      ): Primitive.Number[SFloat] =
        Number(value = Value.Number.Float(minimum, maximum, multiple), metadata = Metadata.Empty)

      override def int(
          minimum: Option[Comparison[SInt]],
          maximum: Option[Comparison[SInt]],
          multiple: Option[SInt]
      ): Primitive.Number[SInt] =
        Number(value = Value.Number.Int(minimum, maximum, multiple), metadata = Metadata.Empty)

      override def long(
          minimum: Option[Comparison[SLong]],
          maximum: Option[Comparison[SLong]],
          multiple: Option[SLong]
      ): Primitive.Number[SLong] =
        Number(value = Value.Number.Long(minimum, maximum, multiple), metadata = Metadata.Empty)

      override def imap[A, B](fa: Primitive.Number[A])(f: A => B)(g: B => A): Primitive.Number[B] =
        fa.copy(value = fa.value.imap(f)(g))

      override def enriched[A]: Enriched[Primitive.Number[A]] = new Enriched[Primitive.Number[A]]:
        override def metadata(a: Primitive.Number[A]): Metadata = a.metadata
        override def modifyMetadata(a: Primitive.Number[A])(f: Metadata => Metadata): Primitive.Number[A] =
          a.copy(metadata = f(a.metadata))

  final case class String[A](value: Primitive.Value.String[A], metadata: Metadata) extends Primitive[A]

  object String:
    given PrimitiveSchemaInvariant.String[Primitive.String] with
      override def parser[A](
          name: JString,
          decode: JString => Either[JString, A],
          encode: A => JString
      ): Primitive.String[A] = String(
        value = Primitive.Value.String.Parser(name, decode, encode),
        metadata = Metadata.Empty
      )

      override def string(
          minimum: Option[SInt],
          maximum: Option[SInt],
          matches: Option[Pattern]
      ): Primitive.String[JString] = String(
        value = Primitive.Value.String.Text(minimum, maximum, matches),
        metadata = Metadata.Empty
      )

      override def imap[A, B](fa: Primitive.String[A])(f: A => B)(g: B => A): Primitive.String[B] =
        fa.copy(value = fa.value.imap(f)(g))

      override def enriched[A]: Enriched[Primitive.String[A]] = new Enriched[Primitive.String[A]]:
        override def metadata(a: Primitive.String[A]): Metadata = a.metadata
        override def modifyMetadata(a: Primitive.String[A])(f: Metadata => Metadata): Primitive.String[A] =
          a.copy(metadata = f(a.metadata))

  def apply[A](value: Primitive.Value[A], metadata: Metadata = Metadata.Empty): Primitive[A] = value match
    case value: Primitive.Value.Boolean[A] => Boolean(value, metadata)
    case value: Primitive.Value.Number[A]  => Number(value, metadata)
    case value: Primitive.Value.String[A]  => String(value, metadata)

  given PrimitiveSchemaInvariant[Primitive] = ???

  sealed abstract class Value[A] extends Product, Serializable:
    def mapK[S[_] >: Nothing, T[_]](fK: [A] => S[A] => T[A]): Value[A]
    def imap[B](f: A => B)(g: B => A): Value[B]

  object Value:
    sealed abstract class Boolean[A] extends Value[A]:
      final override def mapK[S[_] >: Nothing, T[_]](fK: [A] => S[A] => T[A]): Value.Boolean[A] = this
      override def imap[B](f: A => B)(g: B => A): Value.Boolean[B] = Boolean.Modify(self = this, f, g)

    object Boolean:
      final private[otter] case class Modify[A, B](self: Value.Boolean[A], f: A => B, g: B => A)
          extends Value.Boolean[B]

      private[otter] case object Root extends Value.Boolean[SBoolean]

    sealed abstract class Number[A] extends Value[A]:
      final override def mapK[S[_] >: Nothing, T[_]](fK: [A] => S[A] => T[A]): Value.Number[A] = this
      final override def imap[B](f: A => B)(g: B => A): Value.Number[B] = Number.Modify(self = this, f, g)

    object Number:
      final private[otter] case class BigDecimal(
          minimum: Option[Comparison[JBigDecimal]],
          maximum: Option[Comparison[JBigDecimal]],
          multiple: Option[JBigDecimal]
      ) extends Value.Number[JBigDecimal]

      final private[otter] case class BigInteger(
          minimum: Option[Comparison[JBigInteger]],
          maximum: Option[Comparison[JBigInteger]],
          multiple: Option[JBigInteger]
      ) extends Value.Number[JBigInteger]

      final private[otter] case class Double(
          minimum: Option[Comparison[SDouble]],
          maximum: Option[Comparison[SDouble]],
          multiple: Option[SDouble]
      ) extends Value.Number[SDouble]

      final private[otter] case class Float(
          minimum: Option[Comparison[SFloat]],
          maximum: Option[Comparison[SFloat]],
          multiple: Option[SFloat]
      ) extends Value.Number[SFloat]

      final private[otter] case class Int(
          minimum: Option[Comparison[SInt]],
          maximum: Option[Comparison[SInt]],
          multiple: Option[SInt]
      ) extends Value.Number[SInt]

      final private[otter] case class Long(
          minimum: Option[Comparison[SLong]],
          maximum: Option[Comparison[SLong]],
          multiple: Option[SLong]
      ) extends Value.Number[SLong]

      final private[otter] case class Modify[A, B](
          self: Value.Number[A],
          f: A => B,
          g: B => A
      ) extends Value.Number[B]

    sealed abstract class String[A] extends Value[A]:
      final override def mapK[S[_] >: Nothing, T[_]](fK: [A] => S[A] => T[A]): Value.String[A] = this
      final override def imap[B](f: A => B)(g: B => A): Value.String[B] = String.Modify(self = this, f, g)

    object String:
      final private[otter] case class Parsed[A](self: Primitive.Value[A]) extends Value.String[A]

      final private[otter] case class Parser[A](
          name: JString,
          decode: JString => Either[JString, A],
          encode: A => JString
      ) extends Value.String[A]

      final private[otter] case class Text(
          minimum: Option[SInt],
          maximum: Option[SInt],
          matches: Option[Pattern]
      ) extends Value.String[JString]

      final private[otter] case class Modify[A, B](
          self: Value.String[A],
          f: A => B,
          g: B => A
      ) extends Value.String[B]
