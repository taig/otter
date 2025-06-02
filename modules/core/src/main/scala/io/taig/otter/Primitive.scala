package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.operation.Enriched
import io.taig.otter.operation.PrimitiveSchemaInvariant
import io.taig.otter.syntax.EnrichedSyntax.*

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.regex.Pattern
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong

sealed abstract class Primitive[+S[_], A]:
  def value: Primitive.Value[S, A]
  def metadata: Metadata

object Primitive:
  final case class Boolean[A](value: Primitive.Value.Boolean[A], metadata: Metadata) extends Primitive[Nothing, A]

  object Boolean:
    given schema: PrimitiveSchemaInvariant.Boolean[Primitive.Boolean] with
      override val boolean: Primitive.Boolean[SBoolean] =
        Boolean(value = Value.Boolean.Root, metadata = Metadata.Empty)
      override def imap[A, B](fa: Primitive.Boolean[A])(f: A => B)(g: B => A): Primitive.Boolean[B] =
        fa.copy(value = fa.value.imap(f)(g))

      override def enriched[A]: Enriched[Primitive.Boolean[A]] = new Enriched[Primitive.Boolean[A]]:
        override def metadata(a: Boolean[A]): Metadata = a.metadata
        override def modifyMetadata(a: Boolean[A])(f: Metadata => Metadata): Boolean[A] =
          a.copy(metadata = f(a.metadata))

  final case class Number[A](value: Primitive.Value.Number[A], metadata: Metadata) extends Primitive[Nothing, A]

  object Number:
    given schema: PrimitiveSchemaInvariant.Number[Primitive.Number] with
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

  final case class String[+S[_], A](value: Primitive.Value.String[S, A], metadata: Metadata) extends Primitive[S, A]

  object String:
    given schema[Value[_]]: PrimitiveSchemaInvariant.String[Primitive.String[Value, *], Value] with
      override def parser[A](
          name: JString,
          decode: JString => Either[JString, A],
          encode: A => JString
      ): Primitive.String[Value, A] = String(
        value = Primitive.Value.String.Parser(name, decode, encode),
        metadata = Metadata.Empty
      )

      override def string(
          minimum: Option[SInt],
          maximum: Option[SInt],
          matches: Option[Pattern]
      ): Primitive.String[Value, JString] = String(
        value = Primitive.Value.String.Text(minimum, maximum, matches),
        metadata = Metadata.Empty
      )

      extension [A](schema: Value[A])
        override def parsed: String[Value, A] = Primitive.String(
          value = Primitive.Value.String.Parsed(schema),
          metadata = Metadata.Empty
        )

      override def imap[A, B](fa: Primitive.String[Value, A])(f: A => B)(g: B => A): Primitive.String[Value, B] =
        fa.copy(value = fa.value.imap(f)(g))

      override def enriched[A]: Enriched[Primitive.String[Value, A]] = new Enriched[Primitive.String[Value, A]]:
        override def metadata(a: Primitive.String[Value, A]): Metadata = a.metadata
        override def modifyMetadata(a: Primitive.String[Value, A])(
            f: Metadata => Metadata
        ): Primitive.String[Value, A] =
          a.copy(metadata = f(a.metadata))

  given [Value[_]]: PrimitiveSchemaInvariant[Primitive[Value, *], Value] =
    new PrimitiveSchemaInvariant[Primitive[Value, *], Value]:
      self =>
      private val string = Primitive.String.schema[Value]

      export Boolean.schema.boolean
      export Number.schema.{double, float, int, jBigDecimal, jBigInteger, long}
      export string.{parsed, parser, string}

      override def imap[A, B](fa: Primitive[Value, A])(f: A => B)(g: B => A): Primitive[Value, B] = fa match
        case self: Primitive.Boolean[A]       => self.imap(f)(g)
        case self: Primitive.Number[A]        => self.imap(f)(g)
        case self: Primitive.String[Value, A] => self.imap(f)(g)

      override def enriched[A]: Enriched[Primitive[Value, A]] = new Enriched[Primitive[Value, A]]:
        override def metadata(a: Primitive[Value, A]): Metadata = a.metadata
        override def modifyMetadata(a: Primitive[Value, A])(f: Metadata => Metadata): Primitive[Value, A] = a match
          case self: Primitive.Boolean[A]       => self.metadata(f)
          case self: Primitive.Number[A]        => self.metadata(f)
          case self: Primitive.String[Value, A] => self.metadata(f)

  sealed abstract class Value[+S[_], A] extends Product, Serializable:
    def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Value[T, A]
    def imap[B](f: A => B)(g: B => A): Value[S, B]

  object Value:
    sealed abstract class Boolean[A] extends Value[Nothing, A]:
      final override def mapK[S[_] >: Nothing, T[_]](fK: [A] => S[A] => T[A]): Value.Boolean[A] = this
      override def imap[B](f: A => B)(g: B => A): Value.Boolean[B] = Boolean.Modify(self = this, f, g)

    object Boolean:
      final private[otter] case class Modify[A, B](self: Value.Boolean[A], f: A => B, g: B => A)
          extends Value.Boolean[B]

      private[otter] case object Root extends Value.Boolean[SBoolean]

    sealed abstract class Number[A] extends Value[Nothing, A]:
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

    sealed abstract class String[+S[_], A] extends Value[S, A]:
      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Primitive.Value.String[T, A]
      final override def imap[B](f: A => B)(g: B => A): Value.String[S, B] = String.Modify(self = this, f, g)

    object String:
      final private[otter] case class Parsed[S[_], A](self: S[A]) extends Value.String[S, A]:
        override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): String[T, A] = copy(self = fK(self))

      final private[otter] case class Parser[A](
          name: JString,
          decode: JString => Either[JString, A],
          encode: A => JString
      ) extends Value.String[Nothing, A]:
        override def mapK[S[_] >: Nothing, T[_]](fK: [A] => S[A] => T[A]): String[T, A] = this

      final private[otter] case class Text(
          minimum: Option[SInt],
          maximum: Option[SInt],
          matches: Option[Pattern]
      ) extends Value.String[Nothing, JString]:
        override def mapK[S[_] >: Nothing, T[_]](fK: [A] => S[A] => T[A]): String[T, JString] = this

      final private[otter] case class Modify[S[_], A, B](
          self: Value.String[S, A],
          f: A => B,
          g: B => A
      ) extends Value.String[S, B]:
        override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): String[T, B] =
          copy(self = self.mapK[S1, T](fK))
