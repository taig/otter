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
import cats.Id

sealed abstract class Codec[+F <: Data.Any, A] extends Product with Serializable:
  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Codec[F, A]

  def imap[B](f: A => B)(g: B => A): Codec[F, B]

  final def nullable: Optional[F, Option[A]] = Optional.Nullable(codec = this, metadata)
  final def nullable(default: A): Optional[F, A] = Optional.Default(codec = this, value = default, metadata)

object Codec:
  given [F <: Data.Any]: CodecInvariant[Codec[F, *]] with
    override def imap[A, B](fa: Codec[F, A])(f: A => B)(g: B => A): Codec[F, B] = fa.imap(f)(g)

sealed abstract class Collection[F <: Data.Any, A] extends Codec[Data.Array[F], A]:
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

  final private[otter] case class Indexed[F <: Data.Any, A](
      codec: Eval[Codec[F, A]],
      minimum: Option[Int],
      maximum: Option[Int],
      uniqueItems: Boolean,
      metadata: Metadata
  ) extends Collection[F, Vector[A]]:
    override def constraints: Vector[Constraint.Collection] = Collection.constraints(minimum, maximum, uniqueItems)
    override def modifyMetadata(f: Metadata => Metadata): Collection[F, Vector[A]] = copy(metadata = f(metadata))

  final private[otter] case class Linked[F <: Data.Any, A](
      codec: Eval[Codec[F, A]],
      minimum: Option[Int],
      maximum: Option[Int],
      uniqueItems: Boolean,
      metadata: Metadata
  ) extends Collection[F, List[A]]:
    override def constraints: Vector[Constraint.Collection] = Collection.constraints(minimum, maximum, uniqueItems)
    override def modifyMetadata(f: Metadata => Metadata): Collection[F, List[A]] = copy(metadata = f(metadata))

  final private[otter] case class Modify[F <: Data.Any, A, B](self: Collection[F, A], f: A => B, g: B => A)
      extends Collection[F, B]:
    export self.{codec, constraints, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Collection[F, B] = copy(self = self.modifyMetadata(f))

  given [F <: Data.Any]: CodecInvariant[Collection[F, *]] with
    override def imap[A, B](fa: Collection[F, A])(f: A => B)(g: B => A): Collection[F, B] = fa.imap(f)(g)

sealed abstract class Constant[+F <: Data.Primitive, A] extends Codec[F, A]:
  def codec: Eval[Codec[?, ?]]
  def matches(a: A): Boolean
  override def modifyMetadata(f: Metadata => Metadata): Constant[F, A]
  override def imap[B](f: A => B)(g: B => A): Constant[F, B] = Constant.Modify(self = this, f, g)

object Constant:
  final private[otter] case class Modify[F <: Data.Primitive, A, B](self: Constant[F, A], f: A => B, g: B => A)
      extends Constant[F, B]:
    export self.{codec, metadata}
    override def matches(b: B): SBoolean = self.matches(g(b))
    override def modifyMetadata(f: Metadata => Metadata): Constant[F, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Root[F <: Data.Primitive, A: Eq](
      codec: Eval[Codec[F, A]],
      reference: A,
      metadata: Metadata
  ) extends Constant[F, A]:
    override def matches(a: A): SBoolean = reference === a
    override def modifyMetadata(f: Metadata => Metadata): Constant[F, A] = copy(metadata = f(metadata))

  given [F <: Data.Primitive]: CodecInvariant[Constant[F, *]] with
    override def imap[A, B](fa: Constant[F, A])(f: A => B)(g: B => A): Constant[F, B] = fa.imap(f)(g)

sealed abstract class Dictionary[+F <: Data.Any, A] extends Codec[Data.Object[F], A]:
  def constraints: Vector[Constraint.Object]
  override def modifyMetadata(f: Metadata => Metadata): Dictionary[F, A]
  final override def imap[B](f: A => B)(g: B => A): Dictionary[F, B] = Dictionary.Modify(self = this, f, g)

object Dictionary:
  final private[otter] case class Root[F <: Data.Any, A, B](
      key: Codec[Data.Primitive, A],
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

  final private[otter] case class Modify[F <: Data.Any, A, B](self: Dictionary[F, A], f: A => B, g: B => A)
      extends Dictionary[F, B]:
    export self.{constraints, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Dictionary[F, B] = copy(self = self.modifyMetadata(f))

  given [F <: Data.Any]: CodecInvariant[Dictionary[F, *]] with
    override def imap[A, B](fa: Dictionary[F, A])(f: A => B)(g: B => A): Dictionary[F, B] = fa.imap(f)(g)

sealed abstract class Enumeration[+F <: Data.Primitive, A] extends Codec[F, A]:
  def codec: Eval[Codec[?, ?]]
  def values: NonEmptyList[A]
  override def modifyMetadata(f: Metadata => Metadata): Enumeration[F, A]
  override def imap[B](f: A => B)(g: B => A): Enumeration[F, B] = Enumeration.Modify(self = this, f, g)

object Enumeration:
  final private[otter] case class Modify[F <: Data.Primitive, A, B](self: Enumeration[F, A], f: A => B, g: B => A)
      extends Enumeration[F, B]:
    export self.{codec, metadata}
    override def values: NonEmptyList[B] = self.values.map(f)
    override def modifyMetadata(f: Metadata => Metadata): Enumeration[F, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Root[F <: Data.Primitive, A, B](
      codec: Eval[Codec[F, A]],
      mapping: Mapping[B, A],
      metadata: Metadata
  ) extends Enumeration[F, B]:
    override def modifyMetadata(f: Metadata => Metadata): Enumeration[F, B] = copy(metadata = f(metadata))
    override def values: NonEmptyList[B] = mapping.values

  given [F <: Data.Primitive]: CodecInvariant[Enumeration[F, *]] with
    override def imap[A, B](fa: Enumeration[F, A])(f: A => B)(g: B => A): Enumeration[F, B] = fa.imap(f)(g)

sealed abstract class Optional[+F <: Data.Any, A] extends Codec[F | Data.Null, A]:
  override def modifyMetadata(f: Metadata => Metadata): Optional[F, A]
  final override def imap[B](f: A => B)(g: B => A): Optional[F, B] = Optional.Modify(self = this, f, g)

object Optional:
  final private[otter] case class Modify[F <: Data.Any, A, B](self: Optional[F, A], f: A => B, g: B => A)
      extends Optional[F, B]:
    export self.metadata
    override def modifyMetadata(f: Metadata => Metadata): Optional[F, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Default[F <: Data.Any, A](codec: Codec[F, A], value: A, metadata: Metadata)
      extends Optional[F, A]:
    override def modifyMetadata(f: Metadata => Metadata): Optional[F, A] = copy(metadata = f(metadata))

  final private[otter] case class Null(metadata: Metadata) extends Optional[Data.Null, Unit]:
    override def modifyMetadata(f: Metadata => Metadata): Optional[Data.Null, Unit] = copy(metadata = f(metadata))

  final private[otter] case class Nullable[F <: Data.Any, A](codec: Codec[F, A], metadata: Metadata)
      extends Optional[F, Option[A]]:
    override def modifyMetadata(f: Metadata => Metadata): Optional[F, Option[A]] = copy(metadata = f(metadata))

  final private[otter] case class Void[F <: Data.Any, A](metadata: Metadata) extends Optional[F, Unit]:
    override def modifyMetadata(f: Metadata => Metadata): Optional[F, Unit] = copy(metadata = f(metadata))

  given [F <: Data.Primitive]: CodecInvariant[Optional[F, *]] with
    override def imap[A, B](fa: Optional[F, A])(f: A => B)(g: B => A): Optional[F, B] = fa.imap(f)(g)

sealed abstract class Primitive[+F <: Data.Primitive, A] extends Codec[F, A]:
  override def modifyMetadata(f: Metadata => Metadata): Primitive[F, A]
  final override def imap[B](f: A => B)(g: B => A): Primitive[F, B] =
    Primitive.Modify(self = this, f, g)

object Primitive:
  final private[otter] case class BigDecimal(
      minimum: Option[Comparison[JBigDecimal]],
      maximum: Option[Comparison[JBigDecimal]],
      multiple: Option[JBigDecimal],
      metadata: Metadata
  ) extends Primitive[Data.Number, JBigDecimal]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[Data.Number, JBigDecimal] =
      copy(metadata = f(metadata))

  final private[otter] case class BigInteger(
      minimum: Option[Comparison[JBigInteger]],
      maximum: Option[Comparison[JBigInteger]],
      multiple: Option[JBigInteger],
      metadata: Metadata
  ) extends Primitive[Data.Number, JBigInteger]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[Data.Number, JBigInteger] =
      copy(metadata = f(metadata))

  final private[otter] case class Boolean(metadata: Metadata) extends Primitive[SBoolean, SBoolean]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[SBoolean, SBoolean] =
      copy(metadata = f(metadata))

  final private[otter] case class Double(
      minimum: Option[Comparison[SDouble]],
      maximum: Option[Comparison[SDouble]],
      multiple: Option[SDouble],
      metadata: Metadata
  ) extends Primitive[Data.Number, SDouble]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[Data.Number, SDouble] =
      copy(metadata = f(metadata))

  final private[otter] case class Float(
      minimum: Option[Comparison[SFloat]],
      maximum: Option[Comparison[SFloat]],
      multiple: Option[SFloat],
      metadata: Metadata
  ) extends Primitive[Data.Number, SFloat]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[Data.Number, SFloat] =
      copy(metadata = f(metadata))

  final private[otter] case class Int(
      minimum: Option[Comparison[SInt]],
      maximum: Option[Comparison[SInt]],
      multiple: Option[SInt],
      metadata: Metadata
  ) extends Primitive[Data.Number, SInt]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[Data.Number, SInt] =
      copy(metadata = f(metadata))

  final private[otter] case class Long(
      minimum: Option[Comparison[SLong]],
      maximum: Option[Comparison[SLong]],
      multiple: Option[SLong],
      metadata: Metadata
  ) extends Primitive[Data.Number, SLong]:
    override def modifyMetadata(f: Metadata => Metadata): Primitive[Data.Number, SLong] =
      copy(metadata = f(metadata))

  final private[otter] case class Modify[F <: Data.Primitive, A, B](self: Primitive[F, A], f: A => B, g: B => A)
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

  given [F <: Data.Primitive]: CodecInvariant[Primitive[F, *]] with
    override def imap[A, B](fa: Primitive[F, A])(f: A => B)(g: B => A): Primitive[F, B] = fa.imap(f)(g)

sealed abstract class Record[+F <: Data.Any, A] extends Codec[Data.Object[F], A]:
  override def modifyMetadata(f: Metadata => Metadata): Record[F, A]
  final override def imap[B](f: A => B)(g: B => A): Record[F, B] = Record.Modify(self = this, f, g)

object Record:
  final private[otter] case class Empty(metadata: Metadata) extends Record[Nothing, Unit]:
    override def modifyMetadata(f: Metadata => Metadata): Record[Nothing, Unit] = copy(metadata = f(metadata))

  final private[otter] case class Modify[F <: Data.Any, A, B](self: Record[F, A], f: A => B, g: B => A)
      extends Record[F, B]:
    export self.metadata
    override def modifyMetadata(f: Metadata => Metadata): Record[F, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Root[F <: Data.Any, A](field: Field[F, A], metadata: Metadata) extends Record[F, A]:
    override def modifyMetadata(f: Metadata => Metadata): Record[F, A] = copy(metadata = f(metadata))

  final private[otter] case class Zip[F <: Data.Any, A, G <: Data.Any, B](
      left: Record[F, A],
      right: Record[G, B],
      metadata: Metadata
  ) extends Record[F | G, (A, B)]:
    override def modifyMetadata(f: Metadata => Metadata): Record[F | G, (A, B)] = copy(metadata = f(metadata))

  given [F <: Data.Any]: CodecInvariant[Record[F, *]] with
    override def imap[A, B](fa: Record[F, A])(f: A => B)(g: B => A): Record[F, B] = fa.imap(f)(g)

sealed abstract class Tuple[+F <: Data.Any, A] extends Codec[Data.Array[F], A]:
  override def modifyMetadata(f: Metadata => Metadata): Tuple[F, A]
  final override def imap[B](f: A => B)(g: B => A): Tuple[F, B] = Tuple.Modify(self = this, f, g)

object Tuple:
  final private[otter] case class Empty(metadata: Metadata) extends Tuple[Nothing, EmptyTuple]:
    override def modifyMetadata(f: Metadata => Metadata): Tuple[Nothing, EmptyTuple] = copy(metadata = f(metadata))

  final private[otter] case class Modify[F <: Data.Any, A, B](self: Tuple[F, A], f: A => B, g: B => A)
      extends Tuple[F, B]:
    export self.metadata
    override def modifyMetadata(f: Metadata => Metadata): Tuple[F, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Prepend[F <: Data.Any, A <: STuple, G <: Data.Any, B](
      self: Tuple[F, A],
      codec: Codec[G, B],
      metadata: Metadata
  ) extends Tuple[F | G, B *: A]:
    override def modifyMetadata(f: Metadata => Metadata): Tuple[F | G, B *: A] = copy(metadata = f(metadata))

  final private[otter] case class Root[F <: Data.Any, A](codec: Codec[F, A], metadata: Metadata) extends Tuple[F, A]:
    override def modifyMetadata(f: Metadata => Metadata): Tuple[F, A] = copy(metadata = f(metadata))

  // Do we even need that at all?
  final private[otter] case class Zip[F <: Data.Any, A, G <: Data.Any, B](
      left: Codec[F, A],
      right: Codec[G, B],
      metadata: Metadata
  ) extends Tuple[F | G, (A, B)]:
    override def modifyMetadata(f: Metadata => Metadata): Tuple[F | G, (A, B)] = copy(metadata = f(metadata))

  given [F <: Data.Any]: CodecInvariant[Tuple[F, *]] with
    override def imap[A, B](fa: Tuple[F, A])(f: A => B)(g: B => A): Tuple[F, B] = fa.imap(f)(g)

sealed abstract class Union[+F[+a <: Data.Any] <: Data.Any, +G <: Data.Any, A] extends Codec[F[G], A]:
  override def modifyMetadata(f: Metadata => Metadata): Union[F, G, A]
  override def imap[B](f: A => B)(g: B => A): Union[F, G, B]

  def orElse[H <: Data.Any, B](codec: Union[?, H, B]): Union[F, G | H, Either[A, B]]

  // final def :+[H <: Data.Any, B](branch: Branch[G, B]): Union[F, G | H, Either[A, B]] = ???
  // orElse(codec = branch.toUnion)

  def untagged: Union.Untagged[G, A]
  def keyed: Union.Tagged[[a <: Data.Any] =>> a, G, A]
  def merged: Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], G, A]
  def nested: Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], G, A]

object Union:
  sealed abstract class Untagged[+F <: Data.Any, A] extends Union[[a <: Data.Any] =>> a, F, A]:
    override def modifyMetadata(f: Metadata => Metadata): Union.Untagged[F, A]
    final override def imap[B](f: A => B)(g: B => A): Union.Untagged[F, B] = Untagged.Modify(self = this, f, g)
    override def orElse[H <: Data.Any, B](codec: Union[?, H, B]): Union.Untagged[F | H, Either[A, B]] =
      Untagged.OrElse(left = this, right = codec.untagged, metadata = Metadata.Empty)
    final override def untagged: Union.Untagged[F, A] = this
    final override def keyed: Union.Tagged[[a <: Data.Any] =>> a, F, A] = Tagged.Keyed(untagged = this)
    final override def merged: Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], F, A] =
      Tagged.Merged(untagged = this)
    final override def nested: Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], F, A] =
      Tagged.Nested(untagged = this)

  object Untagged:
    final private[otter] case class OrElse[F <: Data.Any, A, G <: Data.Any, B](
        left: Union.Untagged[F, A],
        right: Union.Untagged[G, B],
        metadata: Metadata
    ) extends Union.Untagged[F | G, Either[A, B]]:
      override def modifyMetadata(f: Metadata => Metadata): Union.Untagged[F | G, Either[A, B]] =
        copy(metadata = f(metadata))

    final private[otter] case class Root[F <: Data.Any, A](branch: Branch[F, A], metadata: Metadata)
        extends Union.Untagged[F, A]:
      override def modifyMetadata(f: Metadata => Metadata): Union.Untagged[F, A] = copy(metadata = f(metadata))

    final private[otter] case class Modify[F <: Data.Any, A, B](self: Union.Untagged[F, A], f: A => B, g: B => A)
        extends Union.Untagged[F, B]:
      export self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Union.Untagged[F, B] = copy(self = self.modifyMetadata(f))

  sealed abstract class Tagged[+F[+a <: Data.Any] <: Data.Any, +G <: Data.Any, A] extends Union[F, G, A]:
    override def modifyMetadata(f: Metadata => Metadata): Union.Tagged[F, G, A]
    override def imap[B](f: A => B)(g: B => A): Union.Tagged[F, G, B] = ???
    override def orElse[H <: Data.Any, B](codec: Union[?, H, B]): Union.Tagged[F, G | H, Either[A, B]]

  object Tagged:
    final private[otter] case class Keyed[F <: Data.Any, A](untagged: Union.Untagged[F, A])
        extends Union.Tagged[[a <: Data.Any] =>> a, F, A]:
      export untagged.metadata
      override def modifyMetadata(f: Metadata => Metadata): Union.Tagged[[a <: Data.Any] =>> a, F, A] =
        copy(untagged = untagged.modifyMetadata(f))
      override def imap[B](f: A => B)(g: B => A): Union.Tagged[[a <: Data.Any] =>> a, F, B] =
        copy(untagged = untagged.imap(f)(g))
      override def orElse[H <: Data.Any, B](
          codec: Union[?, H, B]
      ): Union.Tagged[[a <: Data.Any] =>> a, F | H, Either[A, B]] =
        Keyed(untagged = untagged.orElse(codec.untagged))
      override def keyed: Union.Tagged[[a <: Data.Any] =>> a, F, A] = this
      override def merged: Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], F, A] = Merged(untagged)
      override def nested: Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], F, A] = Nested(untagged)

    final private[otter] case class Merged[F <: Data.Any, A](untagged: Union.Untagged[F, A])
        extends Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], F, A]:
      export untagged.metadata
      override def modifyMetadata(
          f: Metadata => Metadata
      ): Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], F, A] =
        copy(untagged = untagged.modifyMetadata(f))
      override def imap[B](f: A => B)(g: B => A): Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], F, B] =
        copy(untagged = untagged.imap(f)(g))
      override def orElse[H <: Data.Any, B](
          codec: Union[?, H, B]
      ): Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], F | H, Either[A, B]] =
        ???
      override def keyed: Union.Tagged[[a <: Data.Any] =>> a, F, A] = Keyed(untagged)
      override def merged: Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], F, A] = this
      override def nested: Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], F, A] = Nested(untagged)

    final private[otter] case class Nested[F <: Data.Any, A](untagged: Union.Untagged[F, A])
        extends Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], F, A]:
      export untagged.metadata
      override def modifyMetadata(
          f: Metadata => Metadata
      ): Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], F, A] =
        copy(untagged = untagged.modifyMetadata(f))
      override def imap[B](f: A => B)(g: B => A): Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], F, B] =
        copy(untagged = untagged.imap(f)(g))
      override def orElse[H <: Data.Any, B](
          codec: Union[?, H, B]
      ): Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], F | H, Either[A, B]] =
        ???
      override def keyed: Union.Tagged[[a <: Data.Any] =>> a, F, A] = Keyed(untagged)
      override def merged: Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], F, A] = Merged(untagged)
      override def nested: Union.Tagged[[a <: Data.Any] =>> Data.Object[String | a], F, A] = this

  // object Tagged:
  // private [otter] final case class Keyed[F <: Data.Any, A](untagged)

  // final case class Tagged[+F[+a <: Data.Any] <: Data.Any, +G <: Data.Any, A](untagged: Union.Untagged[G, A]) extends Union[F, G, A]:
  //   export untagged.metadata
  //   override def modifyMetadata(f: Metadata => Metadata): Union.Tagged[F, G, A] = copy(untagged = untagged.modifyMetadata(f))
  //   override def imap[B](f: A => B)(g: B => A): Union.Tagged[F, G, B] = copy(untagged = untagged.imap(f)(g))

// extension [F <: Data.Any, A <: Matchable](self: Union[F, A])
//   inline def |[G <: Data.Any, B <: Matchable](branch: Branch[G, B]): Union[F | G, A | B] =
//     (self :+ branch).imap {
//       case Left(a)  => a
//       case Right(b) => b
//     } {
//       case a: A => Left(a)
//       case b: B => Right(b)
//     }

// final private[otter] case class Modify[F <: Data.Any, A, B](self: Union[F, A], f: A => B, g: B => A)
//     extends Union[F, B]:
//   export self.metadata
//   override def modifyMetadata(f: Metadata => Metadata): Union[F, B] = copy(self = self.modifyMetadata(f))

// final private[otter] case class Root[F <: Data.Any, A](branch: Branch[F, A], metadata: Metadata) extends Union[F, A]:
//   override def modifyMetadata(f: Metadata => Metadata): Union[F, A] = copy(metadata = f(metadata))

// final private[otter] case class OrElse[F <: Data.Any, A, G <: Data.Any, B](
//     left: Union[F, A],
//     right: Union[G, B],
//     metadata: Metadata
// ) extends Union[F | G, Either[A, B]]:
//   override def modifyMetadata(f: Metadata => Metadata): Union[F | G, Either[A, B]] = copy(metadata = f(metadata))

// given [F <: Data.Any]: CodecInvariant[Union[F, *]] with
//   override def imap[A, B](fa: Union[F, A])(f: A => B)(g: B => A): Union[F, B] = fa.imap(f)(g)
