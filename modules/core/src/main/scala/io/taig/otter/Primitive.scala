package io.taig.otter

import cats.syntax.all.*

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.regex.Pattern
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong
import scala.Tuple as STuple
import cats.data.NonEmptyList
import io.taig.enumeration.ext.Mapping
import cats.Eq

sealed abstract class Primitive[A] extends Codec[Nothing, A]:
  override def modifyMetadata(f: Metadata => Metadata): Primitive[A]
  final override def imap[B](f: A => B)(g: B => A): Primitive[B] =
    Primitive.Modify(self = this, f, g)

object Primitive:
  final private[otter] case class BigDecimal(
      minimum: Option[Comparison[JBigDecimal]],
      maximum: Option[Comparison[JBigDecimal]],
      multiple: Option[JBigDecimal],
      metadata: Metadata
  ) extends Primitive[JBigDecimal]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[JBigDecimal] =
      copy(metadata = f(metadata))

  final private[otter] case class BigInteger(
      minimum: Option[Comparison[JBigInteger]],
      maximum: Option[Comparison[JBigInteger]],
      multiple: Option[JBigInteger],
      metadata: Metadata
  ) extends Primitive[JBigInteger]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[JBigInteger] =
      copy(metadata = f(metadata))

  final private[otter] case class Boolean(metadata: Metadata) extends Primitive[SBoolean]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[SBoolean] =
      copy(metadata = f(metadata))

  final private[otter] case class Double(
      minimum: Option[Comparison[SDouble]],
      maximum: Option[Comparison[SDouble]],
      multiple: Option[SDouble],
      metadata: Metadata
  ) extends Primitive[SDouble]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[SDouble] =
      copy(metadata = f(metadata))

  final private[otter] case class Float(
      minimum: Option[Comparison[SFloat]],
      maximum: Option[Comparison[SFloat]],
      multiple: Option[SFloat],
      metadata: Metadata
  ) extends Primitive[SFloat]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[SFloat] =
      copy(metadata = f(metadata))

  final private[otter] case class Int(
      minimum: Option[Comparison[SInt]],
      maximum: Option[Comparison[SInt]],
      multiple: Option[SInt],
      metadata: Metadata
  ) extends Primitive[SInt]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[SInt] =
      copy(metadata = f(metadata))

  final private[otter] case class Long(
      minimum: Option[Comparison[SLong]],
      maximum: Option[Comparison[SLong]],
      multiple: Option[SLong],
      metadata: Metadata
  ) extends Primitive[SLong]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[SLong] =
      copy(metadata = f(metadata))

  final private[otter] case class Modify[A, B](self: Primitive[A], f: A => B, g: B => A) extends Primitive[B]:
    export self.metadata
    override def modifyMetadata(f: Metadata => Metadata): Primitive[B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Parser[A](
      name: JString,
      decode: JString => Either[JString, A],
      encode: A => JString,
      minimum: Option[SInt],
      maximum: Option[SInt],
      matches: Option[Pattern],
      metadata: Metadata
  ) extends Primitive[A]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[A] = copy(metadata = f(metadata))

  final private[otter] case class String(
      minimum: Option[SInt],
      maximum: Option[SInt],
      matches: Option[Pattern],
      metadata: Metadata
  ) extends Primitive[JString]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[JString] =
      copy(metadata = f(metadata))

  given CodecInvariant[Primitive] with
    override def imap[A, B](fa: Primitive[A])(f: A => B)(g: B => A): Primitive[B] = fa.imap(f)(g)
