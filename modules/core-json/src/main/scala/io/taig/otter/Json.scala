package io.taig.otter

import io.taig.otter as Self

sealed abstract class Json[A] extends Product with Serializable

object Json:
  final case class Collection[A](value: Self.Collection[Json, A]) extends Json[A]

  object Collection:
    given invariant: CollectionInvariant[Json.Collection, Json] with
      override def lift[A](codec: Self.Collection[Json, A]): Collection[A] = Collection(codec)
      override def extract[A](self: Collection[A]): Self.Collection[Json, A] = self.value

  final case class Constant[A](value: Self.Constant[Json.Primitive, A]) extends Json[A]

  final case class Dictionary[A](value: Self.Dictionary[Json.Key, Json, A]) extends Json[A]

  object Dictionary:
    given invariant: DictionaryInvariant[Json.Dictionary, Json.Key, Json] with
      override def lift[A](codec: Self.Dictionary[Key, Json, A]): Json.Dictionary[A] = Json.Dictionary(codec)
      override def extract[A](codec: Json.Dictionary[A]): Self.Dictionary[Key, Json, A] = codec.value

  final case class Enumeration[A](value: Self.Enumeration[Json.Primitive, A]) extends Json[A]

  object Enumeration:
    given invariant: EnumerationInvariant[Json.Enumeration, Json.Primitive] with
      override def lift[A](codec: Self.Enumeration[Primitive, A]): Json.Enumeration[A] = Enumeration(codec)
      override def extract[A](codec: Json.Enumeration[A]): Self.Enumeration[Primitive, A] = codec.value

  final case class Optional[A](value: Self.Optional[Json, A]) extends Json[A]

  object Optional:
    given invariant: OptionalInvariant[Json.Optional, Json] with
      override def lift[A](codec: Self.Optional[Json, A]): Json.Optional[A] = Optional(value = codec)
      override def extract[A](self: Json.Optional[A]): Self.Optional[Json, A] = self.value

  sealed abstract class Primitive[A] extends Json[A]:
    def value: Self.Primitive.Boolean[A] | Self.Primitive.Number[A] | Self.Primitive.String[A]

  object Primitive:
    final case class Boolean[A](value: Self.Primitive.Boolean[A]) extends Json.Primitive[A]

    object Boolean:
      given invariant: PrimitiveInvariant.Boolean[Json.Primitive.Boolean] with
        override def lift[A](codec: Self.Primitive.Boolean[A]): Json.Primitive.Boolean[A] =
          Json.Primitive.Boolean(codec)
        override def extract[A](self: Json.Primitive.Boolean[A]): Self.Primitive.Boolean[A] = self.value

    final case class Number[A](value: Self.Primitive.Number[A]) extends Json.Primitive[A]

    object Number:
      given invariant: PrimitiveInvariant.Number[Json.Primitive.Number] with
        override def lift[A](codec: Self.Primitive.Number[A]): Json.Primitive.Number[A] = Number(codec)
        override def extract[A](self: Json.Primitive.Number[A]): Self.Primitive.Number[A] = self.value

    final case class String[A](value: Self.Primitive.String[A]) extends Json.Primitive[A]

    object String:
      given invariant: PrimitiveInvariant.String[Json.Primitive.String] with
        override def lift[A](codec: Self.Primitive.String[A]): Json.Primitive.String[A] = String(codec)
        override def extract[A](self: Json.Primitive.String[A]): Self.Primitive.String[A] = self.value

  final case class Record[A](value: Self.Record[Json.Key, Json, A]) extends Json[A]

  object Record:
    given RecordInvariant[Json.Record, Json.Key, Json] with
      override def lift[A](codec: Self.Record[Key, Json, A]): Json.Record[A] = Record(codec)
      override def extract[A](codec: Json.Record[A]): Self.Record[Key, Json, A] = codec.value

  final case class Tuple[A](value: Self.Tuple[Json, A]) extends Json[A]

  final case class Union[A](value: Self.Union[Json.Key, Json, A]) extends Json[A]

  type Key[A] = Json.Primitive.String[A]

  type Field[A] = Self.Field[Json.Key, Json, A]

  type Branch[A] = Self.Branch[Json.Key, Json, A]

  given CodecInvariant.Nullable[Json, Json.Optional] = new CodecInvariant.Nullable[Json, Json.Optional]:
    extension [A](self: Json[A])
      override def metadata: Metadata = self match
        case Json.Collection(a)        => a.metadata
        case Json.Constant(a)          => a.metadata
        case Json.Dictionary(a)        => a.metadata
        case Json.Enumeration(a)       => a.metadata
        case Json.Optional(a)          => a.metadata
        case Json.Primitive.Boolean(a) => a.metadata
        case Json.Primitive.Number(a)  => a.metadata
        case Json.Primitive.String(a)  => a.metadata
        case Json.Record(a)            => a.metadata
        case Json.Tuple(a)             => a.metadata
        case Json.Union(a)             => a.metadata

      override def modifyMetadata(f: Metadata => Metadata): Json[A] = self match
        case Json.Collection(a)        => Json.Collection(a.modifyMetadata(f))
        case Json.Constant(a)          => Json.Constant(a.modifyMetadata(f))
        case Json.Dictionary(a)        => Json.Dictionary(a.modifyMetadata(f))
        case Json.Enumeration(a)       => Json.Enumeration(a.modifyMetadata(f))
        case Json.Optional(a)          => Json.Optional(a.modifyMetadata(f))
        case Json.Primitive.Boolean(a) => Json.Primitive.Boolean(a.modifyMetadata(f))
        case Json.Primitive.Number(a)  => Json.Primitive.Number(a.modifyMetadata(f))
        case Json.Primitive.String(a)  => Json.Primitive.String(a.modifyMetadata(f))
        case Json.Record(a)            => Json.Record(a.modifyMetadata(f))
        case Json.Tuple(a)             => Json.Tuple(a.modifyMetadata(f))
        case Json.Union(a)             => Json.Union(a.modifyMetadata(f))

      override def imap[B](f: A => B)(g: B => A): Json[B] = self match
        case Json.Collection(a)        => Json.Collection(a.imap(f)(g))
        case Json.Constant(a)          => Json.Constant(a.imap(f)(g))
        case Json.Dictionary(a)        => Json.Dictionary(a.imap(f)(g))
        case Json.Enumeration(a)       => Json.Enumeration(a.imap(f)(g))
        case Json.Optional(a)          => Json.Optional(a.imap(f)(g))
        case Json.Primitive.Boolean(a) => Json.Primitive.Boolean(a.imap(f)(g))
        case Json.Primitive.Number(a)  => Json.Primitive.Number(a.imap(f)(g))
        case Json.Primitive.String(a)  => Json.Primitive.String(a.imap(f)(g))
        case Json.Record(a)            => Json.Record(a.imap(f)(g))
        case Json.Tuple(a)             => Json.Tuple(a.imap(f)(g))
        case Json.Union(a)             => Json.Union(a.imap(f)(g))
