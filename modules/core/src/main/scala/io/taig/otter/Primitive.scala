package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Metadata

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.regex.Pattern
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong
import io.taig.otter.schema.PrimitiveSchema

sealed abstract class Primitive[A] extends Product with Serializable:
  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Primitive[A]
  def mapK[S[_] >: Nothing, T[_]](fK: [A] => S[A] => T[A]): Primitive[A]
  def imap[B](f: A => B)(g: B => A): Primitive[B]

object Primitive:
  sealed abstract class Boolean[A] extends Primitive[A]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive.Boolean[A]
    final override def mapK[S[_] >: Nothing, T[_]](fK: [A] => S[A] => T[A]): Primitive.Boolean[A] = this
    override def imap[B](f: A => B)(g: B => A): Primitive.Boolean[B] = Boolean.Modify(self = this, f, g)

  object Boolean:
    final private[otter] case class Modify[A, B](self: Primitive.Boolean[A], f: A => B, g: B => A)
        extends Primitive.Boolean[B]:
      export self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Primitive.Boolean[B] = copy(self = self.modifyMetadata(f))

    final private[otter] case class Root(metadata: Metadata) extends Primitive.Boolean[SBoolean]:
      override def modifyMetadata(f: Metadata => Metadata): Primitive.Boolean[SBoolean] =
        copy(metadata = f(metadata))

    given schema: PrimitiveSchema.Boolean[Primitive.Boolean] with
      override def boolean: Boolean[SBoolean] = Root(metadata = Metadata.Empty)

      override def imap[A, B](fa: Primitive.Boolean[A])(f: A => B)(g: B => A): Primitive.Boolean[B] = fa.imap(f)(g)

      extension [A](fa: Boolean[A])
        override def metadata: Metadata = fa.metadata
        override def modifyMetadata(f: Metadata => Metadata): Boolean[A] = fa.modifyMetadata(f)

  sealed abstract class Number[A] extends Primitive[A]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive.Number[A]
    final override def mapK[S[_] >: Nothing, T[_]](fK: [A] => S[A] => T[A]): Primitive.Number[A] = this
    final override def imap[B](f: A => B)(g: B => A): Primitive.Number[B] = Number.Modify(self = this, f, g)

  object Number:
    final private[otter] case class BigDecimal(
        minimum: Option[Comparison[JBigDecimal]],
        maximum: Option[Comparison[JBigDecimal]],
        multiple: Option[JBigDecimal],
        metadata: Metadata
    ) extends Primitive.Number[JBigDecimal]:
      override def modifyMetadata(f: Metadata => Metadata): Primitive.Number[JBigDecimal] =
        copy(metadata = f(metadata))

    final private[otter] case class BigInteger(
        minimum: Option[Comparison[JBigInteger]],
        maximum: Option[Comparison[JBigInteger]],
        multiple: Option[JBigInteger],
        metadata: Metadata
    ) extends Primitive.Number[JBigInteger]:
      override def modifyMetadata(f: Metadata => Metadata): Primitive.Number[JBigInteger] =
        copy(metadata = f(metadata))

    final private[otter] case class Double(
        minimum: Option[Comparison[SDouble]],
        maximum: Option[Comparison[SDouble]],
        multiple: Option[SDouble],
        metadata: Metadata
    ) extends Primitive.Number[SDouble]:
      override def modifyMetadata(f: Metadata => Metadata): Primitive.Number[SDouble] =
        copy(metadata = f(metadata))

    final private[otter] case class Float(
        minimum: Option[Comparison[SFloat]],
        maximum: Option[Comparison[SFloat]],
        multiple: Option[SFloat],
        metadata: Metadata
    ) extends Primitive.Number[SFloat]:
      override def modifyMetadata(f: Metadata => Metadata): Primitive.Number[SFloat] =
        copy(metadata = f(metadata))

    final private[otter] case class Int(
        minimum: Option[Comparison[SInt]],
        maximum: Option[Comparison[SInt]],
        multiple: Option[SInt],
        metadata: Metadata
    ) extends Primitive.Number[SInt]:
      override def modifyMetadata(f: Metadata => Metadata): Primitive.Number[SInt] =
        copy(metadata = f(metadata))

    final private[otter] case class Long(
        minimum: Option[Comparison[SLong]],
        maximum: Option[Comparison[SLong]],
        multiple: Option[SLong],
        metadata: Metadata
    ) extends Primitive.Number[SLong]:
      override def modifyMetadata(f: Metadata => Metadata): Primitive.Number[SLong] =
        copy(metadata = f(metadata))

    final private[otter] case class Modify[A, B](
        self: Primitive.Number[A],
        f: A => B,
        g: B => A
    ) extends Primitive.Number[B]:
      export self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Primitive.Number[B] = copy(self = self.modifyMetadata(f))

    given schema: PrimitiveSchema.Number[Primitive.Number] with
      override def jBigDecimal(
          minimum: Option[Comparison[JBigDecimal]],
          maximum: Option[Comparison[JBigDecimal]],
          multiple: Option[JBigDecimal]
      ): Number[JBigDecimal] = BigDecimal(minimum, maximum, multiple, metadata = Metadata.Empty)

      override def jBigInteger(
          minimum: Option[Comparison[JBigInteger]],
          maximum: Option[Comparison[JBigInteger]],
          multiple: Option[JBigInteger]
      ): Number[JBigInteger] = BigInteger(minimum, maximum, multiple, metadata = Metadata.Empty)

      override def double(
          minimum: Option[Comparison[SDouble]],
          maximum: Option[Comparison[SDouble]],
          multiple: Option[SDouble]
      ): Number[SDouble] = Double(minimum, maximum, multiple, metadata = Metadata.Empty)

      override def float(
          minimum: Option[Comparison[SFloat]],
          maximum: Option[Comparison[SFloat]],
          multiple: Option[SFloat]
      ): Number[SFloat] = Float(minimum, maximum, multiple, metadata = Metadata.Empty)

      override def int(
          minimum: Option[Comparison[SInt]],
          maximum: Option[Comparison[SInt]],
          multiple: Option[SInt]
      ): Number[SInt] = Int(minimum, maximum, multiple, metadata = Metadata.Empty)

      override def long(
          minimum: Option[Comparison[SLong]],
          maximum: Option[Comparison[SLong]],
          multiple: Option[SLong]
      ): Number[SLong] = Long(minimum, maximum, multiple, metadata = Metadata.Empty)

      override def imap[A, B](fa: Primitive.Number[A])(f: A => B)(g: B => A): Primitive.Number[B] = fa.imap(f)(g)

      extension [A](self: Number[A])
        override def metadata: Metadata = self.metadata
        override def modifyMetadata(f: Metadata => Metadata): Number[A] = self.modifyMetadata(f)

  sealed abstract class String[A] extends Primitive[A]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive.String[A]
    final override def mapK[S[_] >: Nothing, T[_]](fK: [A] => S[A] => T[A]): Primitive.String[A] = this
    final override def imap[B](f: A => B)(g: B => A): Primitive.String[B] = String.Modify(self = this, f, g)

  object String:
    final private[otter] case class Parser[A](
        name: JString,
        decode: JString => Either[JString, A],
        encode: A => JString,
        minimum: Option[SInt],
        maximum: Option[SInt],
        matches: Option[Pattern],
        metadata: Metadata
    ) extends Primitive.String[A]:
      override def modifyMetadata(f: Metadata => Metadata): Primitive.String[A] = copy(metadata = f(metadata))

    final private[otter] case class Text(
        minimum: Option[SInt],
        maximum: Option[SInt],
        matches: Option[Pattern],
        metadata: Metadata
    ) extends Primitive.String[JString]:
      override def modifyMetadata(f: Metadata => Metadata): Primitive.String[JString] = copy(metadata = f(metadata))

    final private[otter] case class Modify[A, B](
        self: Primitive.String[A],
        f: A => B,
        g: B => A
    ) extends Primitive.String[B]:
      export self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Primitive.String[B] = copy(self = self.modifyMetadata(f))

    given schema: PrimitiveSchema.String[Primitive.String] with
      override def string(
          minimum: Option[SInt],
          maximum: Option[SInt],
          matches: Option[Pattern]
      ): String[JString] = Text(minimum, maximum, matches, metadata = Metadata.Empty)

      override def parser[A](
          name: JString,
          decode: JString => Either[JString, A],
          encode: A => JString,
          minimum: Option[SInt],
          maximum: Option[SInt],
          matches: Option[Pattern]
      ): String[A] = Parser(name, decode, encode, minimum, maximum, matches, metadata = Metadata.Empty)

      override def imap[A, B](fa: Primitive.String[A])(f: A => B)(g: B => A): Primitive.String[B] = fa.imap(f)(g)

      extension [A](self: String[A])
        override def metadata: Metadata = self.metadata
        override def modifyMetadata(f: Metadata => Metadata): String[A] = self.modifyMetadata(f)

  given PrimitiveSchema[Primitive] = new PrimitiveSchema[Primitive]:
    export Primitive.Boolean.schema.boolean
    export Primitive.Number.schema.{double, float, int, jBigDecimal, jBigInteger, long}
    export Primitive.String.schema.{parser, string}

    override def imap[A, B](fa: Primitive[A])(f: A => B)(g: B => A): Primitive[B] = fa.imap(f)(g)

    extension [A](self: Primitive[A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Primitive[A] = self.modifyMetadata(f)
