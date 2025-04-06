package io.taig.otter

import io.taig.otter as Self
import cats.data.NonEmptyList

sealed abstract class Json[A] extends Product with Serializable

object Json:
  final case class Collection[A](self: Self.Collection[Json, A]) extends Json[A]

  object Collection:
    given invariant: CollectionInvariant[Json.Collection, Json] =
      CollectionInvariant[Json.Collection, Json](
        lift = [A] => (codec: Self.Collection[Json, A]) => Collection(self = codec),
        extract = [A] => (codec: Json.Collection[A]) => codec.self
      )

  final case class Constant[A](self: Self.Constant[Json, A]) extends Json[A]

  object Constant:
    given invariant: ConstantInvariant[Json.Constant, Json] = ConstantInvariant(
      lift = [A] => (codec: Self.Constant[Json, A]) => Constant(self = codec),
      extract = [A] => (codec: Json.Constant[A]) => codec.self
    )

  final case class Dictionary[A](self: Self.Dictionary[Json.Key, Json, A]) extends Json[A]

  object Dictionary:
    given invariant: DictionaryInvariant[Json.Dictionary, Json.Key, Json] =
      DictionaryInvariant(
        lift = [A] => (codec: Self.Dictionary[Json.Key, Json, A]) => Dictionary(self = codec),
        extract = [A] => (codec: Json.Dictionary[A]) => codec.self
      )

  final case class Enumeration[A](self: Self.Enumeration[Json.Primitive, A]) extends Json[A]

  object Enumeration:
    given invariant: EnumerationInvariant[Json.Enumeration, Json.Primitive] =
      new EnumerationInvariant.Lift[Json.Enumeration, Json.Primitive]:
        extension [A](self: Json.Enumeration[A])
          override def values: NonEmptyList[A] = self.self.values
          override def metadata: Metadata = self.self.metadata
          override def modifyMetadata(f: Metadata => Metadata): Enumeration[A] =
            Enumeration(self.self.modifyMetadata(f))
          override def imap[B](f: A => B)(g: B => A): Enumeration[B] = Enumeration(self.self.imap(f)(g))

        override def lift[A](codec: Self.Enumeration[Primitive, A]): Json.Enumeration[A] = Enumeration(codec)

  final case class Optional[A](self: Self.Optional[Json, A]) extends Json[A]

  object Optional:
    given invariant: OptionalInvariant[Json.Optional, Json] = OptionalInvariant[Json.Optional, Json](
      lift = [A] => (codec: Self.Optional[Json, A]) => Optional(self = codec),
      extract = [A] => (codec: Json.Optional[A]) => codec.self
    )

  final case class Primitive[A](self: Self.Primitive[A]) extends Json[A]

  object Primitive:
    given invariant: PrimitiveInvariant[Json.Primitive] = new PrimitiveInvariant.Lift[Json.Primitive]:
      override def lift[A](codec: Self.Primitive[A]): Primitive[A] = Primitive(codec)

      extension [A](self: Primitive[A])
        override def metadata: Metadata = self.self.metadata
        override def modifyMetadata(f: Metadata => Metadata): Primitive[A] = Primitive(self.self.modifyMetadata(f))
        override def imap[B](f: A => B)(g: B => A): Json.Primitive[B] = Primitive(self.self.imap(f)(g))

  final case class Record[A](self: Self.Record[Json.Key, Json, A]) extends Json[A]

  object Record:
    given invariant: RecordInvariant[Json.Record, Json.Key, Json] = RecordInvariant(
      lift = [A] => (codec: Self.Record[Json.Key, Json, A]) => Record(codec),
      extract = [A] => (self: Json.Record[A]) => self.self
    )

  final case class Tuple[A](self: Self.Tuple[Json, A]) extends Json[A]

  object Tuple:
    given invariant: TupleInvariant[Json.Tuple, Json] = TupleInvariant(
      lift = [A] => (codec: Self.Tuple[Json, A]) => Tuple(codec),
      extract = [A] => (self: Json.Tuple[A]) => self.self
    )

  final case class Union[A](self: Self.Union[Json, A]) extends Json[A]

  object Union:
    given invariant: UnionInvariant[Json.Union, Json] = UnionInvariant(
      lift = [A] => (codec: Self.Union[Json, A]) => Union(codec),
      extract = [A] => (self: Json.Union[A]) => self.self
    )

  sealed abstract class Key[A] extends Product with Serializable

  object Key:
    final case class Constant[A](self: Self.Constant[Json.Key.Primitive, A]) extends Json.Key[A]

    object Constant:
      given invariant: ConstantInvariant[Json.Key.Constant, Json.Key.Primitive] = ConstantInvariant(
        lift = [A] => (codec: Self.Constant[Json.Key.Primitive, A]) => Constant(codec),
        extract = [A] => (self: Json.Key.Constant[A]) => self.self
      )

    final case class Enumeration[A](self: Self.Enumeration[Json.Key.Primitive, A]) extends Json.Key[A]

    object Enumeration:
      given invariant: EnumerationInvariant[Json.Key.Enumeration, Json.Key.Primitive] =
        new EnumerationInvariant.Lift[Json.Key.Enumeration, Json.Key.Primitive]:
          override def lift[A](codec: Self.Enumeration[Json.Key.Primitive, A]): Json.Key.Enumeration[A] =
            Enumeration(codec)

          extension [A](self: Json.Key.Enumeration[A])
            override def values: NonEmptyList[A] = self.self.values
            override def metadata: Metadata = self.self.metadata
            override def modifyMetadata(f: Metadata => Metadata): Json.Key.Enumeration[A] =
              Enumeration(self.self.modifyMetadata(f))
            override def imap[B](f: A => B)(g: B => A): Json.Key.Enumeration[B] = Enumeration(self.self.imap(f)(g))

    final case class Primitive[A](self: Self.Primitive.String[A]) extends Json.Key[A]

    object Primitive:
      given invariant: PrimitiveInvariant.String[Json.Key.Primitive] =
        new PrimitiveInvariant.String.Lift[Json.Key.Primitive]:
          override def lift[A](codec: Self.Primitive.String[A]): Json.Key.Primitive[A] = Primitive(codec)

          extension [A](self: Json.Key.Primitive[A])
            override def metadata: Metadata = self.self.metadata
            override def modifyMetadata(f: Metadata => Metadata): Json.Key.Primitive[A] = Primitive(
              self.self.modifyMetadata(f)
            )
            override def imap[B](f: A => B)(g: B => A): Json.Key.Primitive[B] = Primitive(self.self.imap(f)(g))

    final case class Union[A](self: Self.Union.Untagged[Json.Key, A]) extends Json.Key[A]

    object Union:
      given invariant: UnionInvariant.Untagged[Json.Key.Union, Json.Key] = UnionInvariant.Untagged(
        lift = [A] => (codec: Self.Union.Untagged[Json.Key, A]) => Union(codec),
        extract = [A] => (self: Json.Key.Union[A]) => self.self
      )

    given invariant: CodecInvariant[Json.Key] = new CodecInvariant[Json.Key]:
      extension [A](self: Key[A])
        override def metadata: Metadata = self match
          case Key.Constant(a)    => a.metadata
          case Key.Enumeration(a) => a.metadata
          case Key.Primitive(a)   => a.metadata
          case Key.Union(a)       => a.metadata
        override def modifyMetadata(f: Metadata => Metadata): Key[A] = self match
          case Key.Constant(a)    => Key.Constant(a.modifyMetadata(f))
          case Key.Enumeration(a) => Key.Enumeration(a.modifyMetadata(f))
          case Key.Primitive(a)   => Key.Primitive(a.modifyMetadata(f))
          case Key.Union(a)       => Key.Union(a.modifyMetadata(f))
        override def imap[B](f: A => B)(g: B => A): Key[B] = self match
          case Key.Constant(a)    => Key.Constant(a.imap(f)(g))
          case Key.Enumeration(a) => Key.Enumeration(a.imap(f)(g))
          case Key.Primitive(a)   => Key.Primitive(a.imap(f)(g))
          case Key.Union(a)       => Key.Union(a.imap(f)(g))

  given invariant: (CodecInvariant.Nullable[Json, Json.Optional] & CodecInvariant.Tupleable[Json, Json.Tuple]) =
    new CodecInvariant.Nullable[Json, Json.Optional] with CodecInvariant.Tupleable[Json, Json.Tuple]:
      override def optional: OptionalInvariant[Optional, Json] = Optional.invariant
      override def tuple: TupleInvariant[Tuple, Json] = Tuple.invariant

      extension [A](self: Json[A])
        override def metadata: Metadata = self match
          case Json.Collection(a)  => a.metadata
          case Json.Constant(a)    => a.metadata
          case Json.Enumeration(a) => a.metadata
          case Json.Dictionary(a)  => a.metadata
          case Json.Optional(a)    => a.metadata
          case Json.Primitive(a)   => a.metadata
          case Json.Record(a)      => a.metadata
          case Json.Tuple(a)       => a.metadata
          case Json.Union(a)       => a.metadata

        override def modifyMetadata(f: Metadata => Metadata): Json[A] = self match
          case Json.Collection(a)  => Json.Collection(a.modifyMetadata(f))
          case Json.Constant(a)    => Json.Constant(a.modifyMetadata(f))
          case Json.Dictionary(a)  => Json.Dictionary(a.modifyMetadata(f))
          case Json.Enumeration(a) => Json.Enumeration(a.modifyMetadata(f))
          case Json.Optional(a)    => Json.Optional(a.modifyMetadata(f))
          case Json.Primitive(a)   => Json.Primitive(a.modifyMetadata(f))
          case Json.Record(a)      => Json.Record(a.modifyMetadata(f))
          case Json.Tuple(a)       => Json.Tuple(a.modifyMetadata(f))
          case Json.Union(a)       => Json.Union(a.modifyMetadata(f))

        override def imap[B](f: A => B)(g: B => A): Json[B] = self match
          case Json.Collection(a)  => Json.Collection(a.imap(f)(g))
          case Json.Constant(a)    => Json.Constant(a.imap(f)(g))
          case Json.Dictionary(a)  => Json.Dictionary(a.imap(f)(g))
          case Json.Enumeration(a) => Json.Enumeration(a.imap(f)(g))
          case Json.Optional(a)    => Json.Optional(a.imap(f)(g))
          case Json.Primitive(a)   => Json.Primitive(a.imap(f)(g))
          case Json.Record(a)      => Json.Record(a.imap(f)(g))
          case Json.Tuple(a)       => Json.Tuple(a.imap(f)(g))
          case Json.Union(a)       => Json.Union(a.imap(f)(g))
