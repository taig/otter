package io.taig.otter

import io.taig.otter as Self

sealed abstract class Json[A] extends Product with Serializable

object Json:
  final case class Collection[A](value: Self.Collection[Json, A]) extends Json[A]

  object Collection:
    given invariant: CollectionInvariant[Json.Collection, Json] =
      CollectionInvariant[Json.Collection, Json](
        lift = [A] => (codec: Self.Collection[Json, A]) => Collection(value = codec),
        extract = [A] => (codec: Json.Collection[A]) => codec.value
      )

  final case class Constant[A](value: Self.Constant[Json.Primitive, A]) extends Json[A]

  object Constant:
    given invariant: ConstantInvariant[Json.Constant, Json.Primitive] =
      ConstantInvariant(
        lift = [A] => (codec: Self.Constant[Json.Primitive, A]) => Constant(value = codec),
        extract = [A] => (codec: Json.Constant[A]) => codec.value
      )

  final case class Dictionary[A](value: Self.Dictionary[Json.Key, Json, A]) extends Json[A]

  object Dictionary:
    given invariant: DictionaryInvariant[Json.Dictionary, Json.Key, Json] =
      DictionaryInvariant(
        lift = [A] => (codec: Self.Dictionary[Json.Key, Json, A]) => Dictionary(value = codec),
        extract = [A] => (codec: Json.Dictionary[A]) => codec.value
      )

  final case class Enumeration[A](value: Self.Enumeration[Json.Primitive, A]) extends Json[A]

  object Enumeration:
    given invariant: EnumerationInvariant[Json.Enumeration, Json.Primitive] = EnumerationInvariant(
      lift = [A] => (codec: Self.Enumeration[Json.Primitive, A]) => Enumeration(value = codec),
      extract = [A] => (codec: Json.Enumeration[A]) => codec.value
    )

  final case class Optional[A](value: Self.Optional[Json, A]) extends Json[A]

  object Optional:
    given invariant: OptionalInvariant[Json.Optional, Json] = OptionalInvariant[Json.Optional, Json](
      lift = [A] => (codec: Self.Optional[Json, A]) => Optional(value = codec),
      extract = [A] => (codec: Json.Optional[A]) => codec.value
    )

  sealed abstract class Primitive[A] extends Json[A]:
    def value: Self.Primitive.Boolean[A] | Self.Primitive.Number[A] | Self.Primitive.String[A]

  object Primitive:
    final case class Boolean[A](value: Self.Primitive.Boolean[A]) extends Json.Primitive[A]

    object Boolean:
      given invariant: PrimitiveInvariant.Boolean[Json.Primitive.Boolean] =
        PrimitiveInvariant.Boolean[Json.Primitive.Boolean](
          lift = [A] => (codec: Self.Primitive.Boolean[A]) => Boolean(codec),
          extract = [A] => (self: Json.Primitive.Boolean[A]) => self.value
        )

    final case class Number[A](value: Self.Primitive.Number[A]) extends Json.Primitive[A]

    object Number:
      given invariant: PrimitiveInvariant.Number[Json.Primitive.Number] =
        PrimitiveInvariant.Number[Json.Primitive.Number](
          lift = [A] => (codec: Self.Primitive.Number[A]) => Number(codec),
          extract = [A] => (self: Json.Primitive.Number[A]) => self.value
        )

    final case class String[A](value: Self.Primitive.String[A]) extends Json.Primitive[A]

    object String:
      given invariant: PrimitiveInvariant.String[Json.Primitive.String] =
        PrimitiveInvariant.String[Json.Primitive.String](
          lift = [A] => (codec: Self.Primitive.String[A]) => String(codec),
          extract = [A] => (self: Json.Primitive.String[A]) => self.value
        )

  final case class Record[A](value: Self.Record[Json, A]) extends Json[A]

  object Record:
    given invariant: RecordInvariant[Json.Record, Json] = RecordInvariant(
      lift = [A] => (codec: Self.Record[Json, A]) => Record(codec),
      extract = [A] => (self: Json.Record[A]) => self.value
    )

  final case class Tuple[A](value: Self.Tuple[Json, A]) extends Json[A]

  object Tuple:
    given invariant: TupleInvariant[Json.Tuple, Json] = TupleInvariant(
      lift = [A] => (codec: Self.Tuple[Json, A]) => Tuple(codec),
      extract = [A] => (self: Json.Tuple[A]) => self.value
    )

  final case class Union[A](value: Self.Union[Json, A]) extends Json[A]

  object Union:
    given invariant: UnionInvariant[Json.Union, Json] = UnionInvariant(
      lift = [A] => (codec: Self.Union[Json, A]) => Union(codec),
      extract = [A] => (self: Json.Union[A]) => self.value
    )

  type Key[A] = Json.Primitive.String[A]

  type Invariant = CodecInvariant.Nullable[Json, Json.Optional] & CodecInvariant.Tupleable[Json, Json.Tuple]

  given invariant: Json.Invariant = new CodecInvariant.Nullable[Json, Json.Optional]
    with CodecInvariant.Tupleable[Json, Json.Tuple]:
    override def optional: OptionalInvariant[Optional, Json] = Optional.invariant
    override def tuple: TupleInvariant[Tuple, Json] = Tuple.invariant

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
