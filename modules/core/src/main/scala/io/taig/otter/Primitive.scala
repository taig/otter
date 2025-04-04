package io.taig.otter

import cats.syntax.all.*
import cats.~>

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.regex.Pattern
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong

sealed abstract class Primitive[A] extends Codec[Nothing, A]:
  override def modifyMetadata(f: Metadata => Metadata): Primitive[A]
  override def mapK[S[_] >: Nothing, T[_]](fK: S ~> T): Primitive[A]
  override def imap[B](f: A => B)(g: B => A): Primitive[B]

object Primitive:
  sealed abstract class Boolean[A] extends Primitive[A]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive.Boolean[A]
    final override def mapK[S[_] >: Nothing, T[_]](fK: S ~> T): Primitive.Boolean[A] = this
    override def imap[B](f: A => B)(g: B => A): Primitive.Boolean[B] = Boolean.Modify(self = this, f, g)

  object Boolean:
    final private[otter] case class Modify[A, B](self: Primitive.Boolean[A], f: A => B, g: B => A)
        extends Primitive.Boolean[B]:
      export self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Primitive.Boolean[B] = copy(self = self.modifyMetadata(f))

    final private[otter] case class Root(metadata: Metadata) extends Primitive.Boolean[SBoolean]:
      override def modifyMetadata(f: Metadata => Metadata): Primitive.Boolean[SBoolean] =
        copy(metadata = f(metadata))

  sealed abstract class Number[A] extends Primitive[A]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive.Number[A]
    final override def mapK[S[_] >: Nothing, T[_]](fK: S ~> T): Primitive.Number[A] = this
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

  sealed abstract class String[A] extends Primitive[A]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive.String[A]
    final override def mapK[S[_] >: Nothing, T[_]](fK: S ~> T): Primitive.String[A] = this
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
