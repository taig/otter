package io.taig.otter

import io.taig.otter as Self

sealed abstract class Json[A] extends Product with Serializable

object Json:
  final case class Collection[A](self: Self.Collection[Json, A]) extends Json[A]

  object Collection:
    given codec: Codec.Collection[Json.Collection, Json] = Codec.Collection(
      lift = [A] => (self: Self.Collection[Json, A]) => Collection(self),
      extract = [A] => (codec: Json.Collection[A]) => codec.self
    )

  final case class Constant[A](self: Self.Constant[Json, A]) extends Json[A]

  object Constant:
    given codec: Codec.Constant[Json.Constant, Json] = Codec.Constant(
      lift = [A] => (self: Self.Constant[Json, A]) => Constant(self),
      extract = [A] => (codec: Json.Constant[A]) => codec.self
    )

  final case class Dictionary[A](self: Self.Dictionary[Json.Key, Json, A]) extends Json[A]

  object Dictionary:
    given codec: Codec.Dictionary[Json.Dictionary, Json.Key, Json] = Codec.Dictionary(
      lift = [A] => (self: Self.Dictionary[Json.Key, Json, A]) => Dictionary(self),
      extract = [A] => (codec: Json.Dictionary[A]) => codec.self
    )

  final case class Enumeration[A](self: Self.Enumeration[Json.Primitive, A]) extends Json[A]

  object Enumeration:
    given codec: Codec.Enumeration[Json.Enumeration, Json.Primitive] = Codec.Enumeration(
      lift = [A] => (self: Self.Enumeration[Json.Primitive, A]) => Enumeration(self),
      extract = [A] => (codec: Json.Enumeration[A]) => codec.self
    )

  final case class Nullable[A](self: Self.Nullable[Json, A]) extends Json[A]

  object Nullable:
    given codec: Codec.Nullable[Json.Nullable, Json] = Codec.Nullable(
      lift = [A] => (self: Self.Nullable[Json, A]) => Nullable(self),
      extract = [A] => (codec: Json.Nullable[A]) => codec.self
    )

  final case class Primitive[A](self: Self.Primitive[A]) extends Json[A]

  object Primitive:
    given codec: Codec.Primitive[Json.Primitive] = Codec.Primitive(
      lift = [A] => (self: Self.Primitive[A]) => Primitive(self),
      extract = [A] => (codec: Json.Primitive[A]) => codec.self
    )

  final case class Record[A](self: Self.Record[Json.Field, A]) extends Json[A]

  object Record:
    given codec: Codec.Record[Json.Record, Json.Field] = Codec.Record(
      lift = [A] => (self: Self.Record[Json.Field, A]) => Record(self),
      extract = [A] => (codec: Json.Record[A]) => codec.self
    )

  final case class Tuple[A](self: Self.Tuple[Json, A]) extends Json[A]

  object Tuple:
    given codec: Codec.Tuple[Json.Tuple, Json] = Codec.Tuple(
      lift = [A] => (self: Self.Tuple[Json, A]) => Tuple(self),
      extract = [A] => (codec: Json.Tuple[A]) => codec.self
    )

  final case class Union[A](self: Self.Union[Json, A]) extends Json[A]

  object Union:
    given codec: Codec.Union[Json.Union, Json] = Codec.Union(
      lift = [A] => (self: Self.Union[Json, A]) => Union(self),
      extract = [A] => (codec: Json.Union[A]) => codec.self
    )

  sealed abstract class Key[A] extends Product with Serializable

  object Key:
    final case class Constant[A](self: Self.Constant[Json.Key.Primitive, A]) extends Json.Key[A]

    object Constant:
      given codec: Codec.Constant[Json.Key.Constant, Json.Key.Primitive] = Codec.Constant(
        lift = [A] => (self: Self.Constant[Json.Key.Primitive, A]) => Constant(self),
        extract = [A] => (codec: Json.Key.Constant[A]) => codec.self
      )

    final case class Enumeration[A](self: Self.Enumeration[Json.Key.Primitive, A]) extends Json.Key[A]

    object Enumeration:
      given codec: Codec.Enumeration[Json.Key.Enumeration, Json.Key.Primitive] = Codec.Enumeration(
        lift = [A] => (self: Self.Enumeration[Json.Key.Primitive, A]) => Enumeration(self),
        extract = [A] => (codec: Json.Key.Enumeration[A]) => codec.self
      )

    final case class Primitive[A](self: Self.Primitive.String[A]) extends Json.Key[A]

    object Primitive:
      given codec: Codec.Primitive.String[Json.Key.Primitive] = Codec.Primitive.String(
        lift = [A] => (self: Self.Primitive.String[A]) => Primitive(self),
        extract = [A] => (codec: Json.Key.Primitive[A]) => codec.self
      )

    final case class Union[A](self: Self.Union.Untagged[Json.Key, A]) extends Json.Key[A]

    object Union:
      given codec: Codec.Union.Untagged[Json.Key.Union, Json.Key] = Codec.Union.Untagged(
        lift = [A] => (self: Self.Union.Untagged[Json.Key, A]) => Union(self),
        extract = [A] => (codec: Json.Key.Union[A]) => codec.self
      )

    given codec: Codec[Json.Key] with
      extension [A](self: Key[A])
        override def metadata: Metadata = self match
          case Key.Constant(self)    => self.metadata
          case Key.Enumeration(self) => self.metadata
          case Key.Primitive(self)   => self.metadata
          case Key.Union(self)       => self.metadata

        override def modifyMetadata(f: Metadata => Metadata): Key[A] = self match
          case Key.Constant(self)    => Constant(self.modifyMetadata(f))
          case Key.Enumeration(self) => Enumeration(self.modifyMetadata(f))
          case Key.Primitive(self)   => Primitive(self.modifyMetadata(f))
          case Key.Union(self)       => Union(self.modifyMetadata(f))

        override def imap[B](f: A => B)(g: B => A): Key[B] = self match
          case Key.Constant(self)    => Constant(self.imap(f)(g))
          case Key.Enumeration(self) => Enumeration(self.imap(f)(g))
          case Key.Primitive(self)   => Primitive(self.imap(f)(g))
          case Key.Union(self)       => Union(self.imap(f)(g))

  type Field[A] = Self.Field[Json.Key, Json, A]

  given codec: Codec.Field[Json.Field, Json.Key, Json, Json.Record] = Codec.Field(
    lift = [A] => (self: Self.Field[Json.Key, Json, A]) => self,
    extract = [A] => (codec: Json.Field[A]) => codec
  )

  given (Codec.Extension.Nullable[Json, Json.Nullable] & Codec.Extension.Tupleable[Json, Json.Tuple]) =
    new Codec.Extension.Nullable[Json, Json.Nullable] with Codec.Extension.Tupleable[Json, Json.Tuple]:
      override def result: Invariant[Tuple] = Json.Tuple.codec
      override inline def fromElement[A](codec: Json[A]): Json[A] = codec

      extension [A](self: Json[A])
        override def metadata: Metadata = self match
          case Json.Collection(self)  => self.metadata
          case Json.Constant(self)    => self.metadata
          case Json.Dictionary(self)  => self.metadata
          case Json.Enumeration(self) => self.metadata
          case Json.Nullable(self)    => self.metadata
          case Json.Primitive(self)   => self.metadata
          case Json.Record(self)      => self.metadata
          case Json.Tuple(self)       => self.metadata
          case Json.Union(self)       => self.metadata

        override def modifyMetadata(f: Metadata => Metadata): Json[A] = self match
          case Json.Collection(self)  => Collection(self.modifyMetadata(f))
          case Json.Constant(self)    => Constant(self.modifyMetadata(f))
          case Json.Dictionary(self)  => Dictionary(self.modifyMetadata(f))
          case Json.Enumeration(self) => Enumeration(self.modifyMetadata(f))
          case Json.Nullable(self)    => Nullable(self.modifyMetadata(f))
          case Json.Primitive(self)   => Primitive(self.modifyMetadata(f))
          case Json.Record(self)      => Record(self.modifyMetadata(f))
          case Json.Tuple(self)       => Tuple(self.modifyMetadata(f))
          case Json.Union(self)       => Union(self.modifyMetadata(f))

        override def imap[B](f: A => B)(g: B => A): Json[B] = self match
          case Json.Collection(self)  => Collection(self.imap(f)(g))
          case Json.Constant(self)    => Constant(self.imap(f)(g))
          case Json.Dictionary(self)  => Dictionary(self.imap(f)(g))
          case Json.Enumeration(self) => Enumeration(self.imap(f)(g))
          case Json.Nullable(self)    => Nullable(self.imap(f)(g))
          case Json.Primitive(self)   => Primitive(self.imap(f)(g))
          case Json.Record(self)      => Record(self.imap(f)(g))
          case Json.Tuple(self)       => Tuple(self.imap(f)(g))
          case Json.Union(self)       => Union(self.imap(f)(g))
