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
import cats.Eval
import cats.data.NonEmptyList
import io.taig.enumeration.ext.Mapping

sealed abstract class Codec[+F <: Format.Any, A] extends Product with Serializable:
  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Codec[F, A]

  def imap[B](f: A => B)(g: B => A): Codec[F, B]

  final def nullable: Optional[F, Option[A]] = Optional.Nullable(codec = this, metadata)
  final def nullable(default: A): Optional[F, A] = Optional.Default(codec = this, value = default, metadata)

object Codec:
  given [F <: Format.Any]: CodecInvariant[Codec[F, *]] with
    override def imap[A, B](fa: Codec[F, A])(f: A => B)(g: B => A): Codec[F, B] = fa.imap(f)(g)

sealed abstract class Collection[F <: Format.Any, A] extends Codec[Format.Array[F], A]:
  def codec: Eval[Codec[?, ?]]
  def constraints: Vector[Constraint.Collection]
  override def modifyMetadata(f: Metadata => Metadata): Collection[F, A]
  final override def imap[B](f: A => B)(g: B => A): Collection[F, B] = Collection.Modify(self = this, f, g)

object Collection:
  private def constraints(
      minimum: Option[Int],
      maximum: Option[Int],
      uniqueItems: Boolean
  ): Vector[Constraint.Collection] = Vector(
    minimum.map(Constraint.Collection.MinItems.apply),
    maximum.map(Constraint.Collection.MaxItems.apply),
    Option.when(uniqueItems)(Constraint.Collection.UniqueItems)
  ).flatten

  final private[otter] case class Indexed[F <: Format.Any, A](
      codec: Eval[Codec[F, A]],
      minimum: Option[Int],
      maximum: Option[Int],
      uniqueItems: Boolean,
      metadata: Metadata
  ) extends Collection[F, Vector[A]]:
    override def constraints: Vector[Constraint.Collection] = Collection.constraints(minimum, maximum, uniqueItems)
    override def modifyMetadata(f: Metadata => Metadata): Collection[F, Vector[A]] = copy(metadata = f(metadata))

  final private[otter] case class Linked[F <: Format.Any, A](
      codec: Eval[Codec[F, A]],
      minimum: Option[Int],
      maximum: Option[Int],
      uniqueItems: Boolean,
      metadata: Metadata
  ) extends Collection[F, List[A]]:
    override def constraints: Vector[Constraint.Collection] = Collection.constraints(minimum, maximum, uniqueItems)
    override def modifyMetadata(f: Metadata => Metadata): Collection[F, List[A]] = copy(metadata = f(metadata))

  final private[otter] case class Modify[F <: Format.Any, A, B](self: Collection[F, A], f: A => B, g: B => A)
      extends Collection[F, B]:
    export self.{codec, constraints, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Collection[F, B] = copy(self = self.modifyMetadata(f))

  given [F <: Format.Any]: CodecInvariant[Collection[F, *]] with
    override def imap[A, B](fa: Collection[F, A])(f: A => B)(g: B => A): Collection[F, B] = fa.imap(f)(g)

sealed abstract class Constant[+F <: Format.Primitive, A] extends Codec[F, A]:
  def codec: Eval[Codec[?, ?]]
  override def modifyMetadata(f: Metadata => Metadata): Constant[F, A]
  override def imap[B](f: A => B)(g: B => A): Constant[F, B] = Constant.Modify(self = this, f, g)

object Constant:
  final private[otter] case class Modify[F <: Format.Primitive, A, B](self: Constant[F, A], f: A => B, g: B => A)
      extends Constant[F, B]:
    export self.{codec, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Constant[F, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Root[F <: Format.Primitive, A](codec: Eval[Codec[F, A]], value: A, metadata: Metadata)
      extends Constant[F, Unit]:
    override def modifyMetadata(f: Metadata => Metadata): Constant[F, Unit] = copy(metadata = f(metadata))

  given [F <: Format.Primitive]: CodecInvariant[Constant[F, *]] with
    override def imap[A, B](fa: Constant[F, A])(f: A => B)(g: B => A): Constant[F, B] = fa.imap(f)(g)

sealed abstract class Dictionary[+F <: Format.Any, A] extends Codec[Format.Object[F], A]:
  def constraints: Vector[Constraint.Object]
  override def modifyMetadata(f: Metadata => Metadata): Dictionary[F, A]
  final override def imap[B](f: A => B)(g: B => A): Dictionary[F, B] = Dictionary.Modify(self = this, f, g)

object Dictionary:
  final private[otter] case class Root[F <: Format.Any, A, B](
      key: Codec[Format.Primitive, A],
      value: Codec[F, B],
      minimum: Option[Int],
      maximum: Option[Int],
      metadata: Metadata
  ) extends Dictionary[F, List[(A, B)]]:
    override def constraints: Vector[Constraint.Object] = Vector(
      minimum.map(Constraint.Object.MinProperties.apply),
      maximum.map(Constraint.Object.MaxProperties.apply)
    ).flatten
    override def modifyMetadata(f: Metadata => Metadata): Dictionary[F, List[(A, B)]] = copy(metadata = f(metadata))

  final private[otter] case class Modify[F <: Format.Any, A, B](self: Dictionary[F, A], f: A => B, g: B => A)
      extends Dictionary[F, B]:
    export self.{constraints, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Dictionary[F, B] = copy(self = self.modifyMetadata(f))

  given [F <: Format.Any]: CodecInvariant[Dictionary[F, *]] with
    override def imap[A, B](fa: Dictionary[F, A])(f: A => B)(g: B => A): Dictionary[F, B] = fa.imap(f)(g)

sealed abstract class Enumeration[+F <: Format.Primitive, A] extends Codec[F, A]:
  def codec: Eval[Codec[?, ?]]
  def values: NonEmptyList[A]
  override def modifyMetadata(f: Metadata => Metadata): Enumeration[F, A]
  override def imap[B](f: A => B)(g: B => A): Enumeration[F, B] = Enumeration.Modify(self = this, f, g)

object Enumeration:
  final private[otter] case class Modify[F <: Format.Primitive, A, B](self: Enumeration[F, A], f: A => B, g: B => A)
      extends Enumeration[F, B]:
    export self.{codec, metadata}
    override def values: NonEmptyList[B] = self.values.map(f)
    override def modifyMetadata(f: Metadata => Metadata): Enumeration[F, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Root[F <: Format.Primitive, A, B](
      codec: Eval[Codec[F, A]],
      mapping: Mapping[B, A],
      metadata: Metadata
  ) extends Enumeration[F, B]:
    override def modifyMetadata(f: Metadata => Metadata): Enumeration[F, B] = copy(metadata = f(metadata))
    override def values: NonEmptyList[B] = mapping.values

  given [F <: Format.Primitive]: CodecInvariant[Enumeration[F, *]] with
    override def imap[A, B](fa: Enumeration[F, A])(f: A => B)(g: B => A): Enumeration[F, B] = fa.imap(f)(g)

sealed abstract class Optional[+F <: Format.Any, A] extends Codec[F | Format.Null, A]:
  override def modifyMetadata(f: Metadata => Metadata): Optional[F, A]
  final override def imap[B](f: A => B)(g: B => A): Optional[F, B] = Optional.Modify(self = this, f, g)

object Optional:
  final private[otter] case class Modify[F <: Format.Any, A, B](self: Optional[F, A], f: A => B, g: B => A)
      extends Optional[F, B]:
    export self.metadata
    override def modifyMetadata(f: Metadata => Metadata): Optional[F, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Default[F <: Format.Any, A](codec: Codec[F, A], value: A, metadata: Metadata)
      extends Optional[F, A]:
    override def modifyMetadata(f: Metadata => Metadata): Optional[F, A] = copy(metadata = f(metadata))

  final private[otter] case class Null(metadata: Metadata) extends Optional[Format.Null, Unit]:
    override def modifyMetadata(f: Metadata => Metadata): Optional[Format.Null, Unit] = copy(metadata = f(metadata))

  final private[otter] case class Nullable[F <: Format.Any, A](codec: Codec[F, A], metadata: Metadata)
      extends Optional[F, Option[A]]:
    override def modifyMetadata(f: Metadata => Metadata): Optional[F, Option[A]] = copy(metadata = f(metadata))

  final private[otter] case class Void[F <: Format.Any, A](metadata: Metadata) extends Optional[F, Unit]:
    override def modifyMetadata(f: Metadata => Metadata): Optional[F, Unit] = copy(metadata = f(metadata))

  given [F <: Format.Primitive]: CodecInvariant[Optional[F, *]] with
    override def imap[A, B](fa: Optional[F, A])(f: A => B)(g: B => A): Optional[F, B] = fa.imap(f)(g)

sealed abstract class Primitive[+F <: Format.Primitive, A] extends Codec[F, A]:
  override def modifyMetadata(f: Metadata => Metadata): Primitive[F, A]
  final override def imap[B](f: A => B)(g: B => A): Primitive[F, B] =
    Primitive.Modify(self = this, f, g)

object Primitive:
  final private[otter] case class BigDecimal(
      minimum: Option[Comparison[JBigDecimal]],
      maximum: Option[Comparison[JBigDecimal]],
      multiple: Option[JBigDecimal],
      metadata: Metadata
  ) extends Primitive[Format.Number, JBigDecimal]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[Format.Number, JBigDecimal] =
      copy(metadata = f(metadata))

  final private[otter] case class BigInteger(
      minimum: Option[Comparison[JBigInteger]],
      maximum: Option[Comparison[JBigInteger]],
      multiple: Option[JBigInteger],
      metadata: Metadata
  ) extends Primitive[Format.Number, JBigInteger]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[Format.Number, JBigInteger] =
      copy(metadata = f(metadata))

  final private[otter] case class Boolean(metadata: Metadata) extends Primitive[Format.Boolean, SBoolean]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[Format.Boolean, SBoolean] =
      copy(metadata = f(metadata))

  final private[otter] case class Double(
      minimum: Option[Comparison[SDouble]],
      maximum: Option[Comparison[SDouble]],
      multiple: Option[SDouble],
      metadata: Metadata
  ) extends Primitive[Format.Number, SDouble]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[Format.Number, SDouble] =
      copy(metadata = f(metadata))

  final private[otter] case class Float(
      minimum: Option[Comparison[SFloat]],
      maximum: Option[Comparison[SFloat]],
      multiple: Option[SFloat],
      metadata: Metadata
  ) extends Primitive[Format.Number, SFloat]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[Format.Number, SFloat] =
      copy(metadata = f(metadata))

  final private[otter] case class Int(
      minimum: Option[Comparison[SInt]],
      maximum: Option[Comparison[SInt]],
      multiple: Option[SInt],
      metadata: Metadata
  ) extends Primitive[Format.Number, SInt]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[Format.Number, SInt] =
      copy(metadata = f(metadata))

  final private[otter] case class Long(
      minimum: Option[Comparison[SLong]],
      maximum: Option[Comparison[SLong]],
      multiple: Option[SLong],
      metadata: Metadata
  ) extends Primitive[Format.Number, SLong]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[Format.Number, SLong] =
      copy(metadata = f(metadata))

  final private[otter] case class Modify[F <: Format.Primitive, A, B](self: Primitive[F, A], f: A => B, g: B => A)
      extends Primitive[F, B]:
    export self.metadata
    override def modifyMetadata(f: Metadata => Metadata): Primitive[F, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Parser[A](
      name: JString,
      decode: JString => Either[JString, A],
      encode: A => JString,
      minimum: Option[SInt],
      maximum: Option[SInt],
      matches: Option[Pattern],
      metadata: Metadata
  ) extends Primitive[Format.String, A]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[Format.String, A] = copy(metadata = f(metadata))

  final private[otter] case class String(
      minimum: Option[SInt],
      maximum: Option[SInt],
      matches: Option[Pattern],
      metadata: Metadata
  ) extends Primitive[Format.String, JString]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[Format.String, JString] =
      copy(metadata = f(metadata))

  given [F <: Format.Primitive]: CodecInvariant[Primitive[F, *]] with
    override def imap[A, B](fa: Primitive[F, A])(f: A => B)(g: B => A): Primitive[F, B] = fa.imap(f)(g)

sealed abstract class Record[+F <: Format.Any, A] extends Codec[Format.Object[F], A]:
  override def modifyMetadata(f: Metadata => Metadata): Record[F, A]
  final override def imap[B](f: A => B)(g: B => A): Record[F, B] = Record.Modify(self = this, f, g)

object Record:
  final private[otter] case class Empty(metadata: Metadata) extends Record[Nothing, Unit]:
    override def modifyMetadata(f: Metadata => Metadata): Record[Nothing, Unit] = copy(metadata = f(metadata))

  final private[otter] case class Modify[F <: Format.Any, A, B](self: Record[F, A], f: A => B, g: B => A)
      extends Record[F, B]:
    export self.metadata
    override def modifyMetadata(f: Metadata => Metadata): Record[F, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Root[F <: Format.Any, A](field: Field[F, A], metadata: Metadata) extends Record[F, A]:
    override def modifyMetadata(f: Metadata => Metadata): Record[F, A] = copy(metadata = f(metadata))

  final private[otter] case class Zip[F <: Format.Any, A, G <: Format.Any, B](
      left: Record[F, A],
      right: Record[G, B],
      metadata: Metadata
  ) extends Record[F | G, (A, B)]:
    override def modifyMetadata(f: Metadata => Metadata): Record[F | G, (A, B)] = copy(metadata = f(metadata))

  given [F <: Format.Any]: CodecInvariant[Record[F, *]] with
    override def imap[A, B](fa: Record[F, A])(f: A => B)(g: B => A): Record[F, B] = fa.imap(f)(g)

sealed abstract class Tuple[+F <: Format.Any, A] extends Codec[Format.Array[F], A]:
  override def modifyMetadata(f: Metadata => Metadata): Tuple[F, A]
  final override def imap[B](f: A => B)(g: B => A): Tuple[F, B] = Tuple.Modify(self = this, f, g)

object Tuple:
  final private[otter] case class Empty(metadata: Metadata) extends Tuple[Nothing, EmptyTuple]:
    override def modifyMetadata(f: Metadata => Metadata): Tuple[Nothing, EmptyTuple] = copy(metadata = f(metadata))

  final private[otter] case class Modify[F <: Format.Any, A, B](self: Tuple[F, A], f: A => B, g: B => A)
      extends Tuple[F, B]:
    export self.metadata
    override def modifyMetadata(f: Metadata => Metadata): Tuple[F, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Prepend[F <: Format.Any, A <: STuple, G <: Format.Any, B](
      self: Tuple[F, A],
      codec: Codec[G, B],
      metadata: Metadata
  ) extends Tuple[F | G, B *: A]:
    override def modifyMetadata(f: Metadata => Metadata): Tuple[F | G, B *: A] = copy(metadata = f(metadata))

  final private[otter] case class Root[F <: Format.Any, A](codec: Codec[F, A], metadata: Metadata) extends Tuple[F, A]:
    override def modifyMetadata(f: Metadata => Metadata): Tuple[F, A] = copy(metadata = f(metadata))

  // Do we even need that at all?
  final private[otter] case class Zip[F <: Format.Any, A, G <: Format.Any, B](
      left: Codec[F, A],
      right: Codec[G, B],
      metadata: Metadata
  ) extends Tuple[F | G, (A, B)]:
    override def modifyMetadata(f: Metadata => Metadata): Tuple[F | G, (A, B)] = copy(metadata = f(metadata))

  given [F <: Format.Any]: CodecInvariant[Tuple[F, *]] with
    override def imap[A, B](fa: Tuple[F, A])(f: A => B)(g: B => A): Tuple[F, B] = fa.imap(f)(g)

sealed abstract class Union[+F <: Format.Any, A] extends Codec[F, A]:
  override def modifyMetadata(f: Metadata => Metadata): Union[F, A]
  final override def imap[B](f: A => B)(g: B => A): Union[F, B] = Union.Modify(self = this, f, g)

  final def orElse[G <: Format.Any, B](codec: Union[G, B]): Union[F | G, Either[A, B]] =
    Union.OrElse(left = this, right = codec, metadata = Metadata.Empty)

  final def :+[G <: Format.Any, B](branch: Branch[G, B]): Union[F | G, Either[A, B]] =
    orElse(codec = branch.toUnion)

object Union:
  extension [F <: Format.Any, A <: Matchable](self: Union[F, A])
    inline def |[G <: Format.Any, B <: Matchable](branch: Branch[G, B]): Union[F | G, A | B] =
      (self :+ branch).imap {
        case Left(a)  => a
        case Right(b) => b
      } {
        case a: A => Left(a)
        case b: B => Right(b)
      }

  final private[otter] case class Modify[F <: Format.Any, A, B](self: Union[F, A], f: A => B, g: B => A)
      extends Union[F, B]:
    export self.metadata
    override def modifyMetadata(f: Metadata => Metadata): Union[F, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Root[F <: Format.Any, A](branch: Branch[F, A], metadata: Metadata)
      extends Union[F, A]:
    override def modifyMetadata(f: Metadata => Metadata): Union[F, A] = copy(metadata = f(metadata))

  final private[otter] case class OrElse[F <: Format.Any, A, G <: Format.Any, B](
      left: Union[F, A],
      right: Union[G, B],
      metadata: Metadata
  ) extends Union[F | G, Either[A, B]]:
    override def modifyMetadata(f: Metadata => Metadata): Union[F | G, Either[A, B]] = copy(metadata = f(metadata))

  given [F <: Format.Any]: CodecInvariant[Union[F, *]] with
    override def imap[A, B](fa: Union[F, A])(f: A => B)(g: B => A): Union[F, B] = fa.imap(f)(g)
