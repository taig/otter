package io.taig.otter

import cats.Eq
import cats.syntax.all.*
import io.taig.enumeration.ext.Mapping
import io.taig.otter as Self

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.regex.Pattern
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong

trait Codec[Self[_]] extends Invariant[Self]:
  extension [A](self: Self[A])
    def metadata: Metadata
    def modifyMetadata(f: Metadata => Metadata): Self[A]
    final def metadata[B](key: Metadata.Key[B]): Option[B] = metadata.get(key)
    final def metadata[B](key: Metadata.Key[B], value: B): Self[A] = modifyMetadata(_.put(key, value))

object Codec:
  object Extension:
    trait Nullable[Self[_], Optional[_]](using optional: Codec.Nullable[Optional, Self]) extends Codec[Self]:
      extension [A](self: Self[A])
        final def nullable: Optional[Option[A]] = optional.nullable(self)
        final def nullable(default: A): Optional[A] = optional.nullable(self, default)

    trait Tupleable[Self[_], Tuple[_]](using tuple: Codec.Tuple[Tuple, Self])
        extends Codec[Self],
          Invariant.Product[Self, Self, Tuple]:
      extension [A](self: Self[A])
        final override def zip[B](codec: Self[B]): Tuple[(A, B)] =
          tuple.one(self).zip(tuple.one(codec))

  trait Collection[Self[_], Value[_]] extends Codec[Self]:
    def linked[A](codec: => Value[A], minimum: Option[Int], maximum: Option[Int], uniqueItems: Boolean): Self[List[A]]
    def indexed[A](
        codec: => Value[A],
        minimum: Option[Int],
        maximum: Option[Int],
        uniqueItems: Boolean
    ): Self[Vector[A]]

  object Collection:
    def apply[Self[_], Value[_]](
        lift: [A] => (self: Self.Collection[Value, A]) => Self[A],
        extract: [A] => (self: Self[A]) => Self.Collection[Value, A]
    ): Codec.Collection[Self, Value] = new Collection[Self, Value]:
      override def linked[A](
          codec: => Value[A],
          minimum: Option[Int],
          maximum: Option[Int],
          uniqueItems: Boolean
      ): Self[List[A]] = lift(
        Self.Collection.Linked(codec = Reference.later(codec), minimum, maximum, uniqueItems, metadata = Metadata.Empty)
      )
      override def indexed[A](
          codec: => Value[A],
          minimum: Option[Int],
          maximum: Option[Int],
          uniqueItems: Boolean
      ): Self[Vector[A]] = lift(
        Self.Collection
          .Indexed(codec = Reference.later(codec), minimum, maximum, uniqueItems, metadata = Metadata.Empty)
      )

      extension [A](self: Self[A])
        override def metadata: Metadata = extract(self).metadata
        override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
        override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))

  trait Constant[Self[_], Value[_]] extends Codec[Self]:
    def constant[A: Eq](codec: => Value[A], value: A): Self[Unit]

  object Constant:
    def apply[Self[_], Value[_]](
        lift: [A] => (self: Self.Constant[Value, A]) => Self[A],
        extract: [A] => (self: Self[A]) => Self.Constant[Value, A]
    ): Codec.Constant[Self, Value] = new Constant[Self, Value]:
      override def constant[A](codec: => Value[A], value: A)(using eq: Eq[A]): Self[Unit] = lift(
        Self.Constant
          .Root(codec = Reference.Constant(self = Reference.later(codec), value), eq, metadata = Metadata.Empty)
      )

      extension [A](self: Self[A])
        override def metadata: Metadata = extract(self).metadata
        override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
        override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))

  trait Dictionary[Self[_], Key[_], Value[_]] extends Codec[Self]:
    def dictionary[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Option[Int],
        maximum: Option[Int]
    ): Self[List[(A, B)]]

  object Dictionary:
    def apply[Self[_], Key[_], Value[_]](
        lift: [A] => (self: Self.Dictionary[Key, Value, A]) => Self[A],
        extract: [A] => (self: Self[A]) => Self.Dictionary[Key, Value, A]
    ): Codec.Dictionary[Self, Key, Value] = new Codec.Dictionary[Self, Key, Value]:
      override def dictionary[A, B](
          key: => Key[A],
          value: => Value[B],
          minimum: Option[Int],
          maximum: Option[Int]
      ): Self[List[(A, B)]] = lift(
        Self.Dictionary.Root(
          key = Reference.later(key),
          value = Reference.later(value),
          minimum,
          maximum,
          metadata = Metadata.Empty
        )
      )

      extension [A](self: Self[A])
        override def metadata: Metadata = extract(self).metadata
        override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
        override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))

  trait Enumeration[Self[_], Value[_]] extends Codec[Self]:
    def enumeration[A, B](codec: => Value[A], mapping: Mapping[B, A]): Self[B]

  object Enumeration:
    def apply[Self[_], Value[_]](
        lift: [A] => (self: Self.Enumeration[Value, A]) => Self[A],
        extract: [A] => (self: Self[A]) => Self.Enumeration[Value, A]
    ): Codec.Enumeration[Self, Value] = new Codec.Enumeration[Self, Value]:
      override def enumeration[A, B](codec: => Value[A], mapping: Mapping[B, A]): Self[B] = lift(
        Self.Enumeration.Root(codec = Reference.later(codec), mapping = mapping, metadata = Metadata.Empty)
      )

      extension [A](self: Self[A])
        override def metadata: Metadata = extract(self).metadata
        override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
        override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))

  trait Nullable[Self[_], Value[_]] extends Codec[Self]:
    def nullable[A](codec: => Value[A]): Self[Option[A]]
    def nullable[A](codec: => Value[A], default: A): Self[A]
    def void: Self[Unit]

  object Nullable:
    def apply[Self[_], Value[_]](
        lift: [A] => (self: Self.Nullable[Value, A]) => Self[A],
        extract: [A] => (self: Self[A]) => Self.Nullable[Value, A]
    ): Codec.Nullable[Self, Value] = new Codec.Nullable[Self, Value]:
      override def nullable[A](codec: => Value[A]): Self[Option[A]] = lift(
        Self.Nullable.Root(reference = Reference.later(codec), metadata = Metadata.Empty)
      )
      override def nullable[A](codec: => Value[A], default: A): Self[A] = lift(
        Self.Nullable.Default(reference = Reference.later(codec), default = default, metadata = Metadata.Empty)
      )
      override def void: Self[Unit] = lift(Self.Nullable.Void(metadata = Metadata.Empty))

      extension [A](self: Self[A])
        override def metadata: Metadata = extract(self).metadata
        override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
        override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))

  trait Primitive[Self[_]]
      extends Codec.Primitive.Boolean[Self],
        Codec.Primitive.Number[Self],
        Codec.Primitive.String[Self]

  object Primitive:
    trait Boolean[Self[_]] extends Codec[Self]:
      def boolean: Self[SBoolean]

    object Boolean:
      def apply[Self[_]](
          lift: [A] => (self: Self.Primitive.Boolean[A]) => Self[A],
          extract: [A] => (self: Self[A]) => Self.Primitive.Boolean[A]
      ): Codec.Primitive.Boolean[Self] = new Codec.Primitive.Boolean[Self]:
        override val boolean: Self[SBoolean] = lift(Self.Primitive.Boolean.Root(metadata = Metadata.Empty))

        extension [A](self: Self[A])
          override def metadata: Metadata = extract(self).metadata
          override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
          override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))

    trait Number[Self[_]] extends Codec[Self]:
      def jBigDecimal(
          minimum: Option[Comparison[JBigDecimal]],
          maximum: Option[Comparison[JBigDecimal]],
          multiple: Option[JBigDecimal]
      ): Self[JBigDecimal]

      def jBigInteger(
          minimum: Option[Comparison[JBigInteger]],
          maximum: Option[Comparison[JBigInteger]],
          multiple: Option[JBigInteger]
      ): Self[JBigInteger]

      def double(
          minimum: Option[Comparison[SDouble]],
          maximum: Option[Comparison[SDouble]],
          multiple: Option[SDouble]
      ): Self[SDouble]

      def float(
          minimum: Option[Comparison[SFloat]],
          maximum: Option[Comparison[SFloat]],
          multiple: Option[SFloat]
      ): Self[SFloat]

      def int(
          minimum: Option[Comparison[SInt]],
          maximum: Option[Comparison[SInt]],
          multiple: Option[SInt]
      ): Self[SInt]

      def long(
          minimum: Option[Comparison[SLong]],
          maximum: Option[Comparison[SLong]],
          multiple: Option[SLong]
      ): Self[SLong]

    object Number:
      def apply[Self[_]](
          lift: [A] => (self: Self.Primitive.Number[A]) => Self[A],
          extract: [A] => (self: Self[A]) => Self.Primitive.Number[A]
      ): Codec.Primitive.Number[Self] = new Codec.Primitive.Number[Self]:
        override def jBigDecimal(
            minimum: Option[Comparison[JBigDecimal]],
            maximum: Option[Comparison[JBigDecimal]],
            multiple: Option[JBigDecimal]
        ): Self[JBigDecimal] =
          lift(Self.Primitive.Number.BigDecimal(minimum, maximum, multiple, metadata = Metadata.Empty))

        override def jBigInteger(
            minimum: Option[Comparison[JBigInteger]],
            maximum: Option[Comparison[JBigInteger]],
            multiple: Option[JBigInteger]
        ): Self[JBigInteger] =
          lift(Self.Primitive.Number.BigInteger(minimum, maximum, multiple, metadata = Metadata.Empty))

        override def double(
            minimum: Option[Comparison[SDouble]],
            maximum: Option[Comparison[SDouble]],
            multiple: Option[SDouble]
        ): Self[SDouble] =
          lift(Self.Primitive.Number.Double(minimum, maximum, multiple, metadata = Metadata.Empty))

        override def float(
            minimum: Option[Comparison[SFloat]],
            maximum: Option[Comparison[SFloat]],
            multiple: Option[SFloat]
        ): Self[SFloat] =
          lift(Self.Primitive.Number.Float(minimum, maximum, multiple, metadata = Metadata.Empty))

        override def int(
            minimum: Option[Comparison[SInt]],
            maximum: Option[Comparison[SInt]],
            multiple: Option[SInt]
        ): Self[SInt] =
          lift(Self.Primitive.Number.Int(minimum, maximum, multiple, metadata = Metadata.Empty))

        override def long(
            minimum: Option[Comparison[SLong]],
            maximum: Option[Comparison[SLong]],
            multiple: Option[SLong]
        ): Self[SLong] =
          lift(Self.Primitive.Number.Long(minimum, maximum, multiple, metadata = Metadata.Empty))

        extension [A](self: Self[A])
          override def metadata: Metadata = extract(self).metadata
          override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
          override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))

    trait String[Self[_]] extends Codec[Self]:
      def string(minimum: Option[SInt], maximum: Option[SInt], matches: Option[Pattern]): Self[JString]

      def parser[A](
          name: JString,
          decode: JString => Either[JString, A],
          encode: A => JString,
          minimum: Option[SInt],
          maximum: Option[SInt],
          matches: Option[Pattern]
      ): Self[A]

    object String:
      def apply[Self[_]](
          lift: [A] => (self: Self.Primitive.String[A]) => Self[A],
          extract: [A] => (self: Self[A]) => Self.Primitive.String[A]
      ): Codec.Primitive.String[Self] = new Codec.Primitive.String[Self]:
        override def string(
            minimum: Option[SInt],
            maximum: Option[SInt],
            matches: Option[Pattern]
        ): Self[JString] =
          lift(Self.Primitive.String.Text(minimum, maximum, matches, metadata = Metadata.Empty))

        override def parser[A](
            name: JString,
            decode: JString => Either[JString, A],
            encode: A => JString,
            minimum: Option[SInt],
            maximum: Option[SInt],
            matches: Option[Pattern]
        ): Self[A] = lift(
          Self.Primitive.String.Parser(name, decode, encode, minimum, maximum, matches, metadata = Metadata.Empty)
        )

        extension [A](self: Self[A])
          override def metadata: Metadata = extract(self).metadata
          override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
          override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))

    def apply[Self[_]](
        lift: [A] => (self: Self.Primitive[A]) => Self[A],
        extract: [A] => (self: Self[A]) => Self.Primitive[A]
    ): Codec.Primitive[Self] = new Codec.Primitive[Self]:
      override def boolean: Self[SBoolean] = lift(Self.Primitive.Boolean.Root(metadata = Metadata.Empty))

      override def jBigDecimal(
          minimum: Option[Comparison[JBigDecimal]],
          maximum: Option[Comparison[JBigDecimal]],
          multiple: Option[JBigDecimal]
      ): Self[JBigDecimal] =
        lift(Self.Primitive.Number.BigDecimal(minimum, maximum, multiple, metadata = Metadata.Empty))

      override def jBigInteger(
          minimum: Option[Comparison[JBigInteger]],
          maximum: Option[Comparison[JBigInteger]],
          multiple: Option[JBigInteger]
      ): Self[JBigInteger] =
        lift(Self.Primitive.Number.BigInteger(minimum, maximum, multiple, metadata = Metadata.Empty))

      override def double(
          minimum: Option[Comparison[SDouble]],
          maximum: Option[Comparison[SDouble]],
          multiple: Option[SDouble]
      ): Self[SDouble] =
        lift(Self.Primitive.Number.Double(minimum, maximum, multiple, metadata = Metadata.Empty))

      override def float(
          minimum: Option[Comparison[SFloat]],
          maximum: Option[Comparison[SFloat]],
          multiple: Option[SFloat]
      ): Self[SFloat] =
        lift(Self.Primitive.Number.Float(minimum, maximum, multiple, metadata = Metadata.Empty))

      override def int(
          minimum: Option[Comparison[SInt]],
          maximum: Option[Comparison[SInt]],
          multiple: Option[SInt]
      ): Self[SInt] =
        lift(Self.Primitive.Number.Int(minimum, maximum, multiple, metadata = Metadata.Empty))

      override def long(
          minimum: Option[Comparison[SLong]],
          maximum: Option[Comparison[SLong]],
          multiple: Option[SLong]
      ): Self[SLong] =
        lift(Self.Primitive.Number.Long(minimum, maximum, multiple, metadata = Metadata.Empty))

      override def string(
          minimum: Option[SInt],
          maximum: Option[SInt],
          matches: Option[Pattern]
      ): Self[JString] =
        lift(Self.Primitive.String.Text(minimum, maximum, matches, metadata = Metadata.Empty))

      override def parser[A](
          name: JString,
          decode: JString => Either[JString, A],
          encode: A => JString,
          minimum: Option[SInt],
          maximum: Option[SInt],
          matches: Option[Pattern]
      ): Self[A] = lift(
        Self.Primitive.String.Parser(name, decode, encode, minimum, maximum, matches, metadata = Metadata.Empty)
      )

      extension [A](self: Self[A])
        override def metadata: Metadata = extract(self).metadata
        override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
        override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))

  trait Record[Self[_], Field[_]] extends Codec[Self], Invariant.Product[Self, Field, Self]:
    final override def result: Invariant[Self] = this

    def record[A](field: => Field[A]): Self[A]

    extension [A](self: Self[A]) def optional: Self[Option[A]]

  object Record:
    def apply[Self[_], Field[_]](
        lift: [A] => (self: Self.Record[Field, A]) => Self[A],
        extract: [A] => (self: Self[A]) => Self.Record[Field, A]
    ): Codec.Record[Self, Field] = new Codec.Record[Self, Field]:
      override def fromElement[A](codec: Field[A]): Self[A] = record(codec)

      final override def record[A](field: => Field[A]): Self[A] =
        lift(Self.Record.Root(field = Reference.later(field), metadata = Metadata.Empty))

      extension [A](self: Self[A])
        override def metadata: Metadata = extract(self).metadata
        override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
        override def optional: Self[Option[A]] = lift(extract(self).optional)
        override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))
        override def zip[B](codec: Self[B]): Self[(A, B)] = lift(extract(self).zip(extract(codec)))

  trait Tuple[Self[_], Value[_]] extends Codec[Self], Invariant.Product[Self, Value, Self]:
    final override def result: Invariant[Self] = this
    final override def fromElement[A](codec: Value[A]): Self[A] = one(codec)

    def empty: Self[Unit]
    def one[A](codec: => Value[A]): Self[A]

  object Tuple:
    def apply[Self[_], Value[_]](
        lift: [A] => (self: Self.Tuple[Value, A]) => Self[A],
        extract: [A] => (self: Self[A]) => Self.Tuple[Value, A]
    ): Codec.Tuple[Self, Value] = new Codec.Tuple[Self, Value]:
      override val empty: Self[Unit] = lift(Self.Tuple.Empty(metadata = Metadata.Empty))

      override def one[A](codec: => Value[A]): Self[A] =
        lift(Self.Tuple.Root(codec = Reference.later(codec), metadata = Metadata.Empty))

      extension [A](self: Self[A])
        override def metadata: Metadata = extract(self).metadata
        override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
        override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))
        override def zip[B](codec: Self[B]): Self[(A, B)] = lift(extract(self).zip(extract(codec)))

  trait Union[Self[_], Value[_]] extends Union.Untagged[Self, Value]:
    extension [A](self: Self[A])
      def discriminator: Option[Discriminator]
      def untagged: Self[A]
      def keyed: Self[A]
      def merged(discriminator: Discriminator.Merged): Self[A]
      def merged: Self[A] = merged(Discriminator.Merged.Default)
      def explicit(discriminator: Discriminator.Explicit): Self[A]
      final def explicit: Self[A] = explicit(Discriminator.Explicit.Default)

  object Union:
    trait Untagged[Self[_], Value[_]] extends Codec[Self], Invariant.Coproduct[Self, Self]:
      final override def result: Invariant[Self] = this

      def branch[A](name: String, codec: => Value[A]): Self[A]

    object Untagged:
      def apply[Self[_], Value[_]](
          lift: [A] => (self: Self.Union.Untagged[Value, A]) => Self[A],
          extract: [A] => (self: Self[A]) => Self.Union.Untagged[Value, A]
      ): Codec.Union.Untagged[Self, Value] = new Untagged[Self, Value]:
        override def branch[A](name: String, codec: => Value[A]): Self[A] = lift(
          Self.Union.Untagged.Branch(codec = Reference.later(codec), name = name, metadata = Metadata.Empty)
        )

        extension [A](self: Self[A])
          override def metadata: Metadata = extract(self).metadata
          override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
          override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))
          override def orElse[B](codec: Self[B]): Self[Either[A, B]] = lift(extract(self).orElse(extract(codec)))

    def apply[Self[_], Value[_]](
        lift: [A] => (self: Self.Union[Value, A]) => Self[A],
        extract: [A] => (self: Self[A]) => Self.Union[Value, A]
    ): Codec.Union[Self, Value] = new Union[Self, Value]:
      override def branch[A](name: String, codec: => Value[A]): Self[A] = lift(
        Self.Union.Untagged.Branch(codec = Reference.later(codec), name = name, metadata = Metadata.Empty)
      )

      extension [A](self: Self[A])
        override def metadata: Metadata = extract(self).metadata
        override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
        override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))
        override def orElse[B](codec: Self[B]): Self[Either[A, B]] = lift(extract(self).orElse(extract(codec)))
        override def discriminator: Option[Discriminator] = extract(self) match
          case _: Self.Union.Untagged[?, ?]   => none
          case codec: Self.Union.Tagged[?, ?] => codec.discriminator.some
        override def untagged: Self[A] = lift(extract(self).untagged)
        override def keyed: Self[A] = lift(extract(self).keyed)
        override def merged(discriminator: Discriminator.Merged): Self[A] =
          lift(extract(self).merged(discriminator))
        override def explicit(discriminator: Discriminator.Explicit): Self[A] =
          lift(extract(self).explicit(discriminator))

  trait Field[Self[_], -Key[_], -Value[_], Record[_]](using record: Invariant[Record])
      extends Codec[Self],
        Invariant.Product[Self, Self, Record]:
    final override def result: Invariant[Record] = record

    def field[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B]

    extension [A](self: Self[A])
      def optional: Self[Option[A]]
      def toRecord: Record[A]

  object Field:
    def apply[Self[_], Key[_], Value[_], Record[_]: Invariant](
        lift: [A] => (self: Self.Field[Key, Value, A]) => Self[A],
        extract: [A] => (self: Self[A]) => Self.Field[Key, Value, A]
    )(using codec: Codec.Record[Record, Self]): Codec.Field[Self, Key, Value, Record] =
      new Codec.Field[Self, Key, Value, Record]:
        final override inline def fromElement[A](codec: Self[A]): Self[A] = codec
        override def field[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B] = lift:
          Self.Field.Root(
            key = Reference.Constant(self = Reference.later(key), value = name),
            value = Reference.later(value),
            metadata = Metadata.Empty
          )

        extension [A](self: Self[A])
          override def metadata: Metadata = extract(self).metadata
          override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
          override def optional: Self[Option[A]] = lift(extract(self).optional)
          override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))
          override def zip[B](codec: Self[B]): Record[(A, B)] = toRecord.zip(codec.toRecord)
          override def toRecord: Record[A] = codec.record(self)
