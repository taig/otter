package io.taig.otter

import io.taig.otter as Self

sealed abstract class Json[A] extends Product with Serializable

object Json:
  final case class Collection[A](self: Self.Collection[Json, A]) extends Json[A]

  object Collection:
    given Codec.Collection[Json.Collection, Json] = Codec.Collection(
      lift = [A] => (self: Self.Collection[Json, A]) => Collection(self),
      extract = [A] => (codec: Json.Collection[A]) => codec.self
    )

  final case class Constant[A](self: Self.Constant[Json, A]) extends Json[A]

  object Constant:
    given Codec.Constant[Json.Constant, Json] = Codec.Constant(
      lift = [A] => (self: Self.Constant[Json, A]) => Constant(self),
      extract = [A] => (codec: Json.Constant[A]) => codec.self
    )

  final case class Dictionary[A](self: Self.Dictionary[Json.Key, Json, A]) extends Json[A]

  object Dictionary:
    given Codec.Dictionary[Json.Dictionary, Json.Key, Json] = Codec.Dictionary(
      lift = [A] => (self: Self.Dictionary[Json.Key, Json, A]) => Dictionary(self),
      extract = [A] => (codec: Json.Dictionary[A]) => codec.self
    )

  final case class Enumeration[A](self: Self.Enumeration[Json.Primitive, A]) extends Json[A]

  object Enumeration:
    given Codec.Enumeration[Json.Enumeration, Json.Primitive] = Codec.Enumeration(
      lift = [A] => (self: Self.Enumeration[Json.Primitive, A]) => Enumeration(self),
      extract = [A] => (codec: Json.Enumeration[A]) => codec.self
    )

  final case class Optional[A](self: Self.Optional[Json, A]) extends Json[A]

  object Optional:
    given Codec.Optional[Json.Optional, Json] = Codec.Optional(
      lift = [A] => (self: Self.Optional[Json, A]) => Optional(self),
      extract = [A] => (codec: Json.Optional[A]) => codec.self
    )

  final case class Primitive[A](self: Self.Primitive[A]) extends Json[A]

  object Primitive:
    given Codec.Primitive[Json.Primitive] = Codec.Primitive(
      lift = [A] => (self: Self.Primitive[A]) => Primitive(self),
      extract = [A] => (codec: Json.Primitive[A]) => codec.self
    )

  final case class Record[A](self: Self.Record[Json.Key, Json, A]) extends Json[A]

  object Record:
    given Codec.Record[Json.Record, Json.Key, Json] = Codec.Record(
      lift = [A] => (self: Self.Record[Json.Key, Json, A]) => Record(self),
      extract = [A] => (codec: Json.Record[A]) => codec.self
    )

  final case class Tuple[A](self: Self.Tuple[Json, A]) extends Json[A]

  final case class Union[A](self: Self.Union[Json, A]) extends Json[A]

  sealed abstract class Key[A] extends Product with Serializable

  object Key:
    final case class Constant[A](self: Self.Constant[Json.Key.Primitive, A]) extends Json.Key[A]
    final case class Enumeration[A](self: Self.Enumeration[Json.Key.Primitive, A]) extends Json.Key[A]
    final case class Primitive[A](self: Self.Primitive.String[A]) extends Json.Key[A]
    final case class Union[A](self: Self.Union.Untagged[Json.Key, A]) extends Json.Key[A]

  given Codec.Nullable[Json, Json.Optional] with
    extension [A](self: Json[A])
      override def imap[B](f: A => B)(g: B => A): Json[B] = ???
      override def metadata: Metadata = ???
      override def modifyMetadata(f: Metadata => Metadata): Json[A] = ???
