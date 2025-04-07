package io.taig.otter

import io.taig.otter as Self

sealed abstract class Json[A] extends Product with Serializable

object Json:
  final case class Collection[A](self: Self.Collection[Json, A]) extends Json[A]

  object Collection:
    given syntax: Self.Collection.Syntax[Json.Collection, Json] = ???

  final case class Constant[A](self: Self.Constant[Json, A]) extends Json[A]

  object Constant:
    given syntax: Self.Constant.Syntax[Json.Constant, Json] = ???

  final case class Dictionary[A](self: Self.Dictionary[Json.Key, Json, A]) extends Json[A]

  object Dictionary:
    given syntax: Self.Dictionary.Syntax[Json.Dictionary, Json.Key, Json] = ???

  final case class Enumeration[A](self: Self.Enumeration[Json.Primitive, A]) extends Json[A]

  object Enumeration:
    given syntax: Self.Enumeration.Syntax[Json.Enumeration, Json.Primitive] = ???

  final case class Optional[A](self: Self.Optional[Json, A]) extends Json[A]

  object Optional:
    given syntax: Self.Optional.Syntax[Json.Optional, Json] = ???

  final case class Primitive[A](self: Self.Primitive[A]) extends Json[A]

  object Primitive:
    given syntax: Self.Primitive.Syntax[Json.Primitive] = ???

  final case class Record[A](self: Self.Record[Json.Key, Json, A]) extends Json[A]

  object Record:
    given syntax: Self.Record.Syntax[Json.Record, Json.Key, Json] = ???

  final case class Tuple[A](self: Self.Tuple[Json, A]) extends Json[A]

  object Tuple:
    given syntax: Self.Tuple.Syntax[Json.Tuple, Json] = ???

  final case class Union[A](self: Self.Union[Json, A]) extends Json[A]

  object Union:
    given syntax: Self.Union.Syntax[Json.Union, Json] = ???

  sealed abstract class Key[A] extends Product with Serializable

  object Key:
    final case class Constant[A](self: Self.Constant[Json.Key.Primitive, A]) extends Json.Key[A]

    object Constant:
      given syntax: Self.Constant.Syntax[Json.Key.Constant, Json.Key.Primitive] = ???

    final case class Enumeration[A](self: Self.Enumeration[Json.Key.Primitive, A]) extends Json.Key[A]

    object Enumeration:
      given syntax: Self.Enumeration.Syntax[Json.Key.Enumeration, Json.Key.Primitive] = ???

    final case class Primitive[A](self: Self.Primitive.String[A]) extends Json.Key[A]

    object Primitive:
      given syntax: Self.Primitive.String.Syntax[Json.Key.Primitive] = ???

    final case class Union[A](self: Self.Union.Untagged[Json.Key, A]) extends Json.Key[A]

    object Union:
      given syntax: Self.Union.Untagged.Syntax[Json.Key.Union, Json.Key] = ???

    given syntax: Self.Codec.Syntax[Json.Key] = new Self.Codec.Syntax[Json.Key]:
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

  given syntax: (Self.Codec.Syntax.Nullable[Json, Json.Optional] & Self.Codec.Syntax.Tupleable[Json, Json.Tuple]) =
    new Self.Codec.Syntax.Nullable[Json, Json.Optional] with Self.Codec.Syntax.Tupleable[Json, Json.Tuple]:
      override def product: Invariant[Json.Tuple] = Tuple.syntax
      override def optional: Self.Optional.Syntax[Json.Optional, Json] = Optional.syntax
      override def tuple: Self.Tuple.Syntax[Json.Tuple, Json] = Tuple.syntax

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
