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
import cats.Eq

sealed abstract class Codec[+S <: Data.Any, A] extends Product with Serializable:
  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Codec[S, A]
  def imap[B](f: A => B)(g: B => A): Codec[S, B]

  // final def nullable: Optional[this.type, Option[A]] = Optional.Nullable(codec = Eval.now(this), metadata)
  // final def nullable(default: A): Optional[this.type, A] =
  //   Optional.Default(codec = Eval.now(this), value = default, metadata)

object Codec:
  given [S <: Data.Any]: CodecInvariant[Codec[S, *]] with
    override def imap[A, B](fa: Codec[S, A])(f: A => B)(g: B => A): Codec[S, B] = fa.imap(f)(g)

sealed abstract class Collection[+S <: Data.Any, A] extends Codec[Data.Array[S], A]:
  def codec: Eval[Codec[S, ?]]
  def constraints: Vector[Constraint.Collection]
  override def modifyMetadata(f: Metadata => Metadata): Collection[S, A]
  final override def imap[B](f: A => B)(g: B => A): Collection[S, B] = Collection.Modify(self = this, f, g)

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

  final private[otter] case class Indexed[S <: Data.Any, A](
      codec: Eval[Codec[S, A]],
      minimum: Option[Int],
      maximum: Option[Int],
      uniqueItems: Boolean,
      metadata: Metadata
  ) extends Collection[S, Vector[A]]:
    override def constraints: Vector[Constraint.Collection] = Collection.constraints(minimum, maximum, uniqueItems)
    override def modifyMetadata(f: Metadata => Metadata): Collection[S, Vector[A]] = copy(metadata = f(metadata))

  final private[otter] case class Linked[S <: Data.Any, A](
      codec: Eval[Codec[S, A]],
      minimum: Option[Int],
      maximum: Option[Int],
      uniqueItems: Boolean,
      metadata: Metadata
  ) extends Collection[S, List[A]]:
    override def constraints: Vector[Constraint.Collection] = Collection.constraints(minimum, maximum, uniqueItems)
    override def modifyMetadata(f: Metadata => Metadata): Collection[S, List[A]] = copy(metadata = f(metadata))

  final private[otter] case class Modify[S <: Data.Any, A, B](self: Collection[S, A], f: A => B, g: B => A)
      extends Collection[S, B]:
    export self.{codec, constraints, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Collection[S, B] = copy(self = self.modifyMetadata(f))

  given [S <: Data.Any]: CodecInvariant[Collection[S, *]] with
    override def imap[A, B](fa: Collection[S, A])(f: A => B)(g: B => A): Collection[S, B] = fa.imap(f)(g)

sealed abstract class Constant[+S <: Data.Any, A] extends Codec[S, A]:
  def codec: Eval[Codec[S, ?]]
  def matches(a: A): Boolean
  override def modifyMetadata(f: Metadata => Metadata): Constant[S, A]
  override def imap[B](f: A => B)(g: B => A): Constant[S, B] = Constant.Modify(self = this, f, g)

object Constant:
  final private[otter] case class Modify[S <: Data.Any, A, B](self: Constant[S, A], f: A => B, g: B => A)
      extends Constant[S, B]:
    export self.{codec, metadata}
    override def matches(b: B): Boolean = self.matches(g(b))
    override def modifyMetadata(f: Metadata => Metadata): Constant[S, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Root[S <: Data.Any, A: Eq](
      codec: Eval[Codec[S, A]],
      reference: A,
      metadata: Metadata
  ) extends Constant[S, A]:
    override def matches(a: A): Boolean = reference === a
    override def modifyMetadata(f: Metadata => Metadata): Constant[S, A] = copy(metadata = f(metadata))

  given [S <: Data.Any]: CodecInvariant[Constant[S, *]] with
    override def imap[A, B](fa: Constant[S, A])(f: A => B)(g: B => A): Constant[S, B] = fa.imap(f)(g)

sealed abstract class Dictionary[+S <: Data.Any, A] extends Codec[S, A]:
  def codec: Eval[Codec[S, ?]]
  def constraints: Vector[Constraint.Object]
  override def modifyMetadata(f: Metadata => Metadata): Dictionary[S, A]
  final override def imap[B](f: A => B)(g: B => A): Dictionary[S, B] = Dictionary.Modify(self = this, f, g)

object Dictionary:
  final private[otter] case class Root[S <: Data.Any, A, B](
      key: Eval[Codec[Data.Primitive, A]],
      value: Eval[Codec[S, B]],
      minimum: Option[Int],
      maximum: Option[Int],
      metadata: Metadata
  ) extends Dictionary[S, List[(A, B)]]:
    override def codec: Eval[Codec[S, B]] = value
    override def constraints: Vector[Constraint.Object] = Vector(
      minimum.map(Constraint.Object.MinProperties.apply),
      maximum.map(Constraint.Object.MaxProperties.apply)
    ).flatten
    override def modifyMetadata(f: Metadata => Metadata): Dictionary[S, List[(A, B)]] = copy(metadata = f(metadata))

  final private[otter] case class Modify[S <: Data.Any, A, B](self: Dictionary[S, A], f: A => B, g: B => A)
      extends Dictionary[S, B]:
    export self.{codec, constraints, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Dictionary[S, B] = copy(self = self.modifyMetadata(f))

  given [S <: Data.Any]: CodecInvariant[Dictionary[S, *]] with
    override def imap[A, B](fa: Dictionary[S, A])(f: A => B)(g: B => A): Dictionary[S, B] = fa.imap(f)(g)

sealed abstract class Enumeration[+S <: Data.Primitive, A] extends Codec[S, A]:
  def codec: Eval[Codec[S, ?]]
  def values: NonEmptyList[A]
  override def modifyMetadata(f: Metadata => Metadata): Enumeration[S, A]
  override def imap[B](f: A => B)(g: B => A): Enumeration[S, B] = Enumeration.Modify(self = this, f, g)

object Enumeration:
  final private[otter] case class Modify[S <: Data.Primitive, A, B](self: Enumeration[S, A], f: A => B, g: B => A)
      extends Enumeration[S, B]:
    export self.{codec, metadata}
    override def values: NonEmptyList[B] = self.values.map(f)
    override def modifyMetadata(f: Metadata => Metadata): Enumeration[S, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Root[S <: Data.Primitive, A, B](
      codec: Eval[Codec[S, A]],
      mapping: Mapping[B, A],
      metadata: Metadata
  ) extends Enumeration[S, B]:
    override def modifyMetadata(f: Metadata => Metadata): Enumeration[S, B] = copy(metadata = f(metadata))
    override def values: NonEmptyList[B] = mapping.values

  given [S <: Data.Primitive]: CodecInvariant[Enumeration[S, *]] with
    override def imap[A, B](fa: Enumeration[S, A])(f: A => B)(g: B => A): Enumeration[S, B] = fa.imap(f)(g)

sealed abstract class Optional[+S <: Data.Any, A] extends Codec[S, A]:
  def codec: Eval[Codec[S, ?]]
  override def modifyMetadata(f: Metadata => Metadata): Optional[S, A]
  final override def imap[B](f: A => B)(g: B => A): Optional[S, B] = Optional.Modify(self = this, f, g)

object Optional:
  final private[otter] case class Modify[S <: Data.Any, A, B](self: Optional[S, A], f: A => B, g: B => A)
      extends Optional[S, B]:
    export self.{codec, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Optional[S, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Default[S <: Data.Any, A](codec: Eval[Codec[S, A]], value: A, metadata: Metadata)
      extends Optional[S, A]:
    override def modifyMetadata(f: Metadata => Metadata): Optional[S, A] = copy(metadata = f(metadata))

  // final private[otter] case class Null(metadata: Metadata) extends Optional[Nothing, Unit]:
  //   override def modifyMetadata(f: Metadata => Metadata): Optional[Nothing, Unit] = copy(metadata = f(metadata))

  final private[otter] case class Nullable[S <: Data.Any, A](codec: Eval[Codec[S, A]], metadata: Metadata)
      extends Optional[S, Option[A]]:
    override def modifyMetadata(f: Metadata => Metadata): Optional[S, Option[A]] = copy(metadata = f(metadata))

  // final private[otter] case class Void[F , A](metadata: Metadata) extends Optional[F, Unit]:
  //   override def modifyMetadata(f: Metadata => Metadata): Optional[F, Unit] = copy(metadata = f(metadata))

  given [S <: Data.Any]: CodecInvariant[Optional[S, *]] with
    override def imap[A, B](fa: Optional[S, A])(f: A => B)(g: B => A): Optional[S, B] = fa.imap(f)(g)

sealed abstract class Primitive[+S <: Data.Primitive, A] extends Codec[S, A]:
  override def modifyMetadata(f: Metadata => Metadata): Primitive[S, A]
  final override def imap[B](f: A => B)(g: B => A): Primitive[S, B] =
    Primitive.Modify(self = this, f, g)

object Primitive:
  final private[otter] case class BigDecimal(
      minimum: Option[Comparison[JBigDecimal]],
      maximum: Option[Comparison[JBigDecimal]],
      multiple: Option[JBigDecimal],
      metadata: Metadata
  ) extends Primitive[JBigDecimal, JBigDecimal]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[JBigDecimal, JBigDecimal] =
      copy(metadata = f(metadata))

  final private[otter] case class BigInteger(
      minimum: Option[Comparison[JBigInteger]],
      maximum: Option[Comparison[JBigInteger]],
      multiple: Option[JBigInteger],
      metadata: Metadata
  ) extends Primitive[JBigInteger, JBigInteger]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[JBigInteger, JBigInteger] =
      copy(metadata = f(metadata))

  final private[otter] case class Boolean(metadata: Metadata) extends Primitive[SBoolean, SBoolean]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[SBoolean, SBoolean] =
      copy(metadata = f(metadata))

  final private[otter] case class Double(
      minimum: Option[Comparison[SDouble]],
      maximum: Option[Comparison[SDouble]],
      multiple: Option[SDouble],
      metadata: Metadata
  ) extends Primitive[SDouble, SDouble]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[SDouble, SDouble] =
      copy(metadata = f(metadata))

  final private[otter] case class Float(
      minimum: Option[Comparison[SFloat]],
      maximum: Option[Comparison[SFloat]],
      multiple: Option[SFloat],
      metadata: Metadata
  ) extends Primitive[SFloat, SFloat]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[SFloat, SFloat] =
      copy(metadata = f(metadata))

  final private[otter] case class Int(
      minimum: Option[Comparison[SInt]],
      maximum: Option[Comparison[SInt]],
      multiple: Option[SInt],
      metadata: Metadata
  ) extends Primitive[SInt, SInt]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[SInt, SInt] =
      copy(metadata = f(metadata))

  final private[otter] case class Long(
      minimum: Option[Comparison[SLong]],
      maximum: Option[Comparison[SLong]],
      multiple: Option[SLong],
      metadata: Metadata
  ) extends Primitive[SLong, SLong]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[SLong, SLong] =
      copy(metadata = f(metadata))

  final private[otter] case class Modify[S <: Data.Primitive, A, B](self: Primitive[S, A], f: A => B, g: B => A)
      extends Primitive[S, B]:
    export self.metadata
    override def modifyMetadata(f: Metadata => Metadata): Primitive[S, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Parser[A](
      name: JString,
      decode: JString => Either[JString, A],
      encode: A => JString,
      minimum: Option[SInt],
      maximum: Option[SInt],
      matches: Option[Pattern],
      metadata: Metadata
  ) extends Primitive[JString, A]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[JString, A] = copy(metadata = f(metadata))

  final private[otter] case class String(
      minimum: Option[SInt],
      maximum: Option[SInt],
      matches: Option[Pattern],
      metadata: Metadata
  ) extends Primitive[JString, JString]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[JString, JString] =
      copy(metadata = f(metadata))

  given [S <: Data.Primitive]: CodecInvariant[Primitive[S, *]] with
    override def imap[A, B](fa: Primitive[S, A])(f: A => B)(g: B => A): Primitive[S, B] = fa.imap(f)(g)

sealed abstract class Record[+S <: Data.Any, A] extends Codec[Data.Object[S], A]:
  def codecs: Vector[Eval[Codec[S, ?]]]
  override def modifyMetadata(f: Metadata => Metadata): Record[S, A]
  final override def imap[B](f: A => B)(g: B => A): Record[S, B] = Record.Modify(self = this, f, g)

object Record:
  final private[otter] case class Empty(metadata: Metadata) extends Record[Nothing, Unit]:
    override def codecs: Vector[Eval[Nothing]] = Vector.empty
    override def modifyMetadata(f: Metadata => Metadata): Record[Nothing, Unit] = copy(metadata = f(metadata))

  final private[otter] case class Modify[S <: Data.Any, A, B](self: Record[S, A], f: A => B, g: B => A)
      extends Record[S, B]:
    export self.{codecs, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Record[S, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Root[S <: Data.Any, A](field: Field[S, A], metadata: Metadata) extends Record[S, A]:
    override def codecs: Vector[Eval[Codec[S, ?]]] = Vector(field.codec)
    override def modifyMetadata(f: Metadata => Metadata): Record[S, A] = copy(metadata = f(metadata))

  final private[otter] case class Zip[S <: Data.Any, T <: Data.Any, A, B](
      left: Record[S, A],
      right: Record[T, B],
      metadata: Metadata
  ) extends Record[S | T, (A, B)]:
    override def codecs: Vector[Eval[Codec[S | T, ?]]] = left.codecs ++ right.codecs
    override def modifyMetadata(f: Metadata => Metadata): Record[S | T, (A, B)] = copy(metadata = f(metadata))

  given [S <: Data.Any]: CodecInvariant[Record[S, *]] with
    override def imap[A, B](fa: Record[S, A])(f: A => B)(g: B => A): Record[S, B] = fa.imap(f)(g)

sealed abstract class Tuple[+S <: Data.Any, A] extends Codec[Data.Array[S], A]:
  def codecs: Vector[Eval[Codec[S, ?]]]
  override def modifyMetadata(f: Metadata => Metadata): Tuple[S, A]
  final override def imap[B](f: A => B)(g: B => A): Tuple[S, B] = Tuple.Modify(self = this, f, g)

object Tuple:
  final private[otter] case class Empty(metadata: Metadata) extends Tuple[Nothing, EmptyTuple]:
    override def codecs: Vector[Eval[Nothing]] = Vector.empty
    override def modifyMetadata(f: Metadata => Metadata): Tuple[Nothing, EmptyTuple] = copy(metadata = f(metadata))

  final private[otter] case class Modify[S <: Data.Any, A, B](self: Tuple[S, A], f: A => B, g: B => A)
      extends Tuple[S, B]:
    export self.{codecs, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Tuple[S, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Prepend[S <: Data.Any, T <: Data.Any, A <: STuple, B](
      self: Tuple[S, A],
      codec: Eval[Codec[T, B]],
      metadata: Metadata
  ) extends Tuple[S | T, B *: A]:
    override def codecs: Vector[Eval[Codec[S | T, ?]]] = codec +: self.codecs
    override def modifyMetadata(f: Metadata => Metadata): Tuple[S | T, B *: A] = copy(metadata = f(metadata))

  final private[otter] case class Root[S <: Data.Any, A](codec: Eval[Codec[S, A]], metadata: Metadata)
      extends Tuple[S, A]:
    override def codecs: Vector[Eval[Codec[S, A]]] = Vector(codec)
    override def modifyMetadata(f: Metadata => Metadata): Tuple[S, A] = copy(metadata = f(metadata))

  // Do we even need that at all?
  // final private[otter] case class Zip[F , A, G , B](
  //     left: Tuple[F, A],
  //     right: Tuple[G, B],
  //     metadata: Metadata
  // ) extends Tuple[F | G, (A, B)]:
  //   override def modifyMetadata(f: Metadata => Metadata): Tuple[F | G, (A, B)] = copy(metadata = f(metadata))

  given [S <: Data.Any]: CodecInvariant[Tuple[S, *]] with
    override def imap[A, B](fa: Tuple[S, A])(f: A => B)(g: B => A): Tuple[S, B] = fa.imap(f)(g)

sealed abstract class Union[+F[+_ <: Data.Any] <: Data.Any, +S <: Data.Any, A] extends Codec[F[S], A]:
  override def modifyMetadata(f: Metadata => Metadata): Union[F, S, A]
  override def imap[B](f: A => B)(g: B => A): Union[F, S, B]

  def orElse[T <: Data.Any, B](codec: Union[?, T, B]): Union[F, S | T, Either[A, B]]

  def :+[T <: Data.Any, B](branch: Branch[T, B]): Union[F, S | T, Either[A, B]]

  def untagged: Union.Untagged[S, A]
  def keyed: Union.Tagged[[a <: Data.Any] =>> a, S, A]
  def merged: Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], S, A]
  def nested: Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], S, A]

object Union:
  sealed abstract class Untagged[+S <: Data.Any, A] extends Union[[a <: Data.Any] =>> a, S, A]:
    override def modifyMetadata(f: Metadata => Metadata): Union.Untagged[S, A]
    final override def imap[B](f: A => B)(g: B => A): Union.Untagged[S, B] = Untagged.Modify(self = this, f, g)
    override def orElse[T <: Data.Any, B](codec: Union[?, T, B]): Union.Untagged[S | T, Either[A, B]] =
      Untagged.OrElse(left = this, right = codec.untagged, metadata = Metadata.Empty)
    override def :+[T <: Data.Any, B](branch: Branch[T, B]): Union.Untagged[S | T, Either[A, B]] =
      orElse(codec = branch.toUnion)
    final override def untagged: Union.Untagged[S, A] = this
    final override def keyed: Union.Tagged[[a <: Data.Any] =>> a, S, A] = Tagged.Keyed(untagged = this)
    final override def merged: Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], S, A] =
      Tagged.Merged(untagged = this, discriminator = Discriminator.Merged.Default)
    final override def nested: Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], S, A] =
      Tagged.Nested(untagged = this, discriminator = Discriminator.Nested.Default)

  object Untagged:
    final private[otter] case class OrElse[S <: Data.Any, T <: Data.Any, A, B](
        left: Union.Untagged[S, A],
        right: Union.Untagged[T, B],
        metadata: Metadata
    ) extends Union.Untagged[S | T, Either[A, B]]:
      override def modifyMetadata(f: Metadata => Metadata): Union.Untagged[S | T, Either[A, B]] =
        copy(metadata = f(metadata))

    final private[otter] case class Root[S <: Data.Any, A](branch: Branch[S, A], metadata: Metadata)
        extends Union.Untagged[S, A]:
      override def modifyMetadata(f: Metadata => Metadata): Union.Untagged[S, A] = copy(metadata = f(metadata))

    final private[otter] case class Modify[S <: Data.Any, A, B](self: Union.Untagged[S, A], f: A => B, g: B => A)
        extends Union.Untagged[S, B]:
      export self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Union.Untagged[S, B] = copy(self = self.modifyMetadata(f))

    extension [S <: Data.Any, A <: Matchable](self: Union.Untagged[S, A])
      inline def |[T <: Data.Any, B <: Matchable](branch: Branch[T, B]): Union.Untagged[S | T, A | B] =
        (self :+ branch).imap {
          case Left(a)  => a
          case Right(b) => b
        } {
          case a: A => Left(a)
          case b: B => Right(b)
        }

  sealed abstract class Tagged[+F[+_ <: Data.Any] <: Data.Any, +S <: Data.Any, A] extends Union[F, S, A]:
    override def modifyMetadata(f: Metadata => Metadata): Union.Tagged[F, S, A]
    override def imap[B](f: A => B)(g: B => A): Union.Tagged[F, S, B]
    override def orElse[T <: Data.Any, B](codec: Union[?, T, B]): Union.Tagged[F, S | T, Either[A, B]]
    override def :+[T <: Data.Any, B](branch: Branch[T, B]): Union.Tagged[F, S | T, Either[A, B]] =
      orElse(codec = branch.toUnion)

  object Tagged:
    final private[otter] case class Keyed[S <: Data.Any, A](untagged: Union.Untagged[S, A])
        extends Union.Tagged[[a <: Data.Any] =>> a, S, A]:
      export untagged.metadata
      override def modifyMetadata(f: Metadata => Metadata): Union.Tagged[[a <: Data.Any] =>> a, S, A] =
        copy(untagged = untagged.modifyMetadata(f))
      override def imap[B](f: A => B)(g: B => A): Union.Tagged[[a <: Data.Any] =>> a, S, B] =
        copy(untagged = untagged.imap(f)(g))
      override def orElse[T <: Data.Any, B](
          codec: Union[?, T, B]
      ): Union.Tagged[[a <: Data.Any] =>> a, S | T, Either[A, B]] =
        Keyed(untagged = untagged.orElse(codec.untagged))
      override def keyed: Union.Tagged[[a <: Data.Any] =>> a, S, A] = this
      override def merged: Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], S, A] =
        Merged(untagged, discriminator = Discriminator.Merged.Default)
      override def nested: Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], S, A] =
        Nested(untagged, discriminator = Discriminator.Nested.Default)

    final private[otter] case class Merged[S <: Data.Any, A](
        untagged: Union.Untagged[S, A],
        discriminator: Discriminator.Merged
    ) extends Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], S, A]:
      export untagged.metadata
      override def modifyMetadata(
          f: Metadata => Metadata
      ): Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], S, A] =
        copy(untagged = untagged.modifyMetadata(f))
      override def imap[B](f: A => B)(g: B => A): Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], S, B] =
        copy(untagged = untagged.imap(f)(g))
      override def orElse[T <: Data.Any, B](
          codec: Union[?, T, B]
      ): Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], S | T, Either[A, B]] =
        copy(untagged = untagged.orElse(codec.untagged))
      override def keyed: Union.Tagged[[a <: Data.Any] =>> a, S, A] = Keyed(untagged)
      override def merged: Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], S, A] = this
      override def nested: Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], S, A] =
        Nested(untagged, discriminator = Discriminator.Nested.Default)

    final private[otter] case class Nested[S <: Data.Any, A](
        untagged: Union.Untagged[S, A],
        discriminator: Discriminator.Nested
    ) extends Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], S, A]:
      export untagged.metadata
      override def modifyMetadata(
          f: Metadata => Metadata
      ): Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], S, A] =
        copy(untagged = untagged.modifyMetadata(f))
      override def imap[B](f: A => B)(g: B => A): Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], S, B] =
        copy(untagged = untagged.imap(f)(g))
      override def orElse[T <: Data.Any, B](
          codec: Union[?, T, B]
      ): Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], S | T, Either[A, B]] =
        copy(untagged = untagged.orElse(codec.untagged))
      override def keyed: Union.Tagged[[a <: Data.Any] =>> a, S, A] = Keyed(untagged)
      override def merged: Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], S, A] =
        Merged(untagged, discriminator = Discriminator.Merged.Default)
      override def nested: Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], S, A] = this

  extension [F[+_ <: Data.Any] <: Data.Any, S <: Data.Any, A <: Matchable](self: Union[F, S, A])
    inline def |[T <: Data.Any, B <: Matchable](branch: Branch[T, B]): Union[F, S | T, A | B] =
      (self :+ branch).imap {
        case Left(a)  => a
        case Right(b) => b
      } {
        case a: A => Left(a)
        case b: B => Right(b)
      }

  given [F[+_ <: Data.Any] <: Data.Any, S <: Data.Any]: CodecInvariant[Union[F, S, *]] with
    override def imap[A, B](fa: Union[F, S, A])(f: A => B)(g: B => A): Union[F, S, B] = fa.imap(f)(g)
