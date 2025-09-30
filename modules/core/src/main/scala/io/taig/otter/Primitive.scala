package io.taig.otter

import cats.syntax.all.*
import cats.~>
import io.taig.otter.operation.Enriched
import io.taig.otter.operation.PrimitiveSchemaInvariant
import io.taig.validation.Constraint
import io.taig.validation.Validation

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong

sealed abstract class Primitive[+S[_], A]:
  def value: Primitive.Value[S, A]
  def metadata: Metadata
  def metadata(f: Metadata => Metadata): Primitive[S, A]

  def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Primitive[T, A]

object Primitive:
  final case class Boolean[A](value: Primitive.Value.Boolean[A], metadata: Metadata) extends Primitive[Nothing, A]:
    override def metadata(f: Metadata => Metadata): Primitive.Boolean[A] = copy(metadata = f(metadata))

    def mapK[S1[_] >: Nothing, T[_]](fK: S1 ~> T): Primitive[T, A] = this

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

  final case class Number[A](value: Primitive.Value.Number[A], metadata: Metadata) extends Primitive[Nothing, A]:
    override def metadata(f: Metadata => Metadata): Primitive.Number[A] = copy(metadata = f(metadata))

    override def mapK[S1[_] >: Nothing, T[_]](fK: S1 ~> T): Primitive[T, A] = this

  object Number:
    given schema: PrimitiveSchemaInvariant.Number[Primitive.Number] with
      override def jBigDecimal(
          validation: Validation[Constraint.Primitive.Number, JBigDecimal]
      ): Primitive.Number[JBigDecimal] = Number(value = Value.Number.BigDecimal(validation), metadata = Metadata.Empty)

      override def jBigInteger(
          validation: Validation[Constraint.Primitive.Number, JBigInteger]
      ): Primitive.Number[JBigInteger] = Number(value = Value.Number.BigInteger(validation), metadata = Metadata.Empty)

      override def double(
          validation: Validation[Constraint.Primitive.Number, SDouble]
      ): Primitive.Number[SDouble] = Number(value = Value.Number.Double(validation), metadata = Metadata.Empty)

      override def float(
          validation: Validation[Constraint.Primitive.Number, SFloat]
      ): Primitive.Number[SFloat] = Number(value = Value.Number.Float(validation), metadata = Metadata.Empty)

      override def int(
          validation: Validation[Constraint.Primitive.Number, SInt]
      ): Primitive.Number[SInt] = Number(value = Value.Number.Int(validation), metadata = Metadata.Empty)

      override def long(
          validation: Validation[Constraint.Primitive.Number, SLong]
      ): Primitive.Number[SLong] = Number(value = Value.Number.Long(validation), metadata = Metadata.Empty)

      override def imap[A, B](fa: Primitive.Number[A])(f: A => B)(g: B => A): Primitive.Number[B] =
        fa.copy(value = fa.value.imap(f)(g))

      override def enriched[A]: Enriched[Primitive.Number[A]] = new Enriched[Primitive.Number[A]]:
        override def metadata(a: Primitive.Number[A]): Metadata = a.metadata
        override def modifyMetadata(a: Primitive.Number[A])(f: Metadata => Metadata): Primitive.Number[A] =
          a.copy(metadata = f(a.metadata))

  final case class String[+S[_], A](value: Primitive.Value.String[S, A], metadata: Metadata) extends Primitive[S, A]:
    override def metadata(f: Metadata => Metadata): Primitive.String[S, A] = copy(metadata = f(metadata))

    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Primitive[T, A] = copy(value = value.mapK(fK))

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
          validation: Validation[Constraint.Primitive.Text, JString]
      ): Primitive.String[Value, JString] = String(
        value = Primitive.Value.String.Text(validation),
        metadata = Metadata.Empty
      )

      override def parsed[A](schema: => Value[A]): String[Value, A] =
        String(value = Primitive.Value.String.Parsed(Reference.later(schema)), metadata = Metadata.Empty)

      override def imap[A, B](fa: Primitive.String[Value, A])(f: A => B)(g: B => A): Primitive.String[Value, B] =
        fa.copy(value = fa.value.imap(f)(g))

      override def enriched[A]: Enriched[Primitive.String[Value, A]] = new Enriched[Primitive.String[Value, A]]:
        override def metadata(a: Primitive.String[Value, A]): Metadata = a.metadata
        override def modifyMetadata(a: Primitive.String[Value, A])(
            f: Metadata => Metadata
        ): Primitive.String[Value, A] = a.copy(metadata = f(a.metadata))

  given [Value[_]]: PrimitiveSchemaInvariant[Primitive[Value, *], Value] with
    val string = String.schema[Value]

    export Boolean.schema.boolean
    export Number.schema.{double, float, int, jBigDecimal, jBigInteger, long}
    export string.{parsed, parser, string}

    override def imap[A, B](fa: Primitive[Value, A])(f: A => B)(g: B => A): Primitive[Value, B] = fa match
      case schema: Boolean[A]       => schema.imap(f)(g)
      case schema: Number[A]        => schema.imap(f)(g)
      case schema: String[Value, A] => schema.imap(f)(g)

    override def enriched[A]: Enriched[Primitive[Value, A]] = new Enriched[Primitive[Value, A]]:
      override def metadata(a: Primitive[Value, A]): Metadata = a.metadata
      override def modifyMetadata(a: Primitive[Value, A])(f: Metadata => Metadata): Primitive[Value, A] =
        a.metadata(f)

  sealed abstract class Value[+S[_], A] extends Product, Serializable:
    def imap[B](f: A => B)(g: B => A): Primitive.Value[S, B]

  object Value:
    sealed abstract class Boolean[A] extends Primitive.Value[Nothing, A]:
      override def imap[B](f: A => B)(g: B => A): Primitive.Value.Boolean[B] = Boolean.Modify(self = this, f, g)

    object Boolean:
      final private[otter] case class Modify[A, B](self: Primitive.Value.Boolean[A], f: A => B, g: B => A)
          extends Primitive.Value.Boolean[B]

      private[otter] case object Root extends Primitive.Value.Boolean[SBoolean]

    sealed abstract class Number[A] extends Primitive.Value[Nothing, A]:
      final override def imap[B](f: A => B)(g: B => A): Primitive.Value.Number[B] = Number.Modify(self = this, f, g)

    object Number:
      final private[otter] case class BigDecimal(
          validation: Validation[Constraint.Primitive.Number, JBigDecimal]
      ) extends Value.Number[JBigDecimal]

      final private[otter] case class BigInteger(
          validation: Validation[Constraint.Primitive.Number, JBigInteger]
      ) extends Value.Number[JBigInteger]

      final private[otter] case class Double(
          validation: Validation[Constraint.Primitive.Number, SDouble]
      ) extends Value.Number[SDouble]

      final private[otter] case class Float(
          validation: Validation[Constraint.Primitive.Number, SFloat]
      ) extends Value.Number[SFloat]

      final private[otter] case class Int(
          validation: Validation[Constraint.Primitive.Number, SInt]
      ) extends Value.Number[SInt]

      final private[otter] case class Long(
          validation: Validation[Constraint.Primitive.Number, SLong]
      ) extends Value.Number[SLong]

      final private[otter] case class Modify[A, B](
          self: Value.Number[A],
          f: A => B,
          g: B => A
      ) extends Value.Number[B]

    sealed abstract class String[+S[_], A] extends Primitive.Value[S, A]:
      def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Primitive.Value.String[T, A]
      final override def imap[B](f: A => B)(g: B => A): Value.String[S, B] = String.Modify(self = this, f, g)

    object String:
      final private[otter] case class Parsed[S[_], A](self: Reference[S, A]) extends Primitive.Value.String[S, A]:
        override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): String[T, A] =
          copy(self = self.mapK[S1, T](fK))

      final private[otter] case class Parser[A](
          name: JString,
          decode: JString => Either[JString, A],
          encode: A => JString
      ) extends Primitive.Value.String[Nothing, A]:
        override def mapK[S[_] >: Nothing, T[_]](fK: S ~> T): String[T, A] = this

      final private[otter] case class Text(
          validation: Validation[Constraint.Primitive.Text, JString]
      ) extends Primitive.Value.String[Nothing, JString]:
        override def mapK[S[_] >: Nothing, T[_]](fK: S ~> T): String[T, JString] = this

      final private[otter] case class Modify[S[_], A, B](
          self: Primitive.Value.String[S, A],
          f: A => B,
          g: B => A
      ) extends Primitive.Value.String[S, B]:
        override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): String[T, B] =
          copy(self = self.mapK[S1, T](fK))
