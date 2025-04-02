package io.taig.otter

import io.taig.otter as Self
import cats.Invariant

sealed abstract class Json[A]:
  def value: Self.Collection[Json, A] | Self.Constant[Json, A] | Self.Dictionary[Json.Key, Json, A] |
    Self.Enumeration[A] | Self.Optional[Json, A] | Self.Primitive[A] | Self.Record[Json.Key, Json, A] |
    Self.Tuple[Json, A] | Self.Union[Json.Key, Json, A]

object Json:
  final case class Collection[A](value: Self.Collection[Json, A]) extends Json[A]
  final case class Constant[A](value: Self.Constant[Json, A]) extends Json[A]

  final case class Dictionary[A](value: Self.Dictionary[Json.Key, Json, A]) extends Json[A]

  final case class Enumeration[A](value: Self.Enumeration[A]) extends Json[A]

  final case class Optional[A](value: Self.Optional[Json, A]) extends Json[A]

  object Optional:
    given OptionalInvariant[Json.Optional, Json] with
      override def lift[A](codec: Self.Optional[Json, A]): Json.Optional[A] = Optional(value = codec)
      override def imap[A, B](fa: Json.Optional[A])(f: A => B)(g: B => A): Json.Optional[B] =
        Json.Optional(fa.value.imap(f)(g))

  sealed abstract class Primitive[A] extends Json[A]

  object Primitive:
    final case class Boolean[A](value: Self.Primitive.Boolean[A]) extends Json.Primitive[A]

    object Boolean:
      given Invariant[Json.Primitive.Boolean] with
        override def imap[A, B](fa: Json.Primitive.Boolean[A])(f: A => B)(g: B => A): Json.Primitive.Boolean[B] =
          Json.Primitive.Boolean(fa.value.imap(f)(g))

    final case class Number[A](value: Self.Primitive.Number[A]) extends Json.Primitive[A]

    object Number:
      given PrimitiveInvariant.Number[Json.Primitive.Number] with
        override def lift[A](codec: Self.Primitive.Number[A]): Json.Primitive.Number[A] = Number(codec)
        override def imap[A, B](fa: Json.Primitive.Number[A])(f: A => B)(g: B => A): Json.Primitive.Number[B] =
          Json.Primitive.Number(fa.value.imap(f)(g))

    final case class String[A](value: Self.Primitive.String[A]) extends Json.Primitive[A]

    object String:
      given PrimitiveInvariant.String[Json.Primitive.String] with
        override def lift[A](codec: Self.Primitive.String[A]): Json.Primitive.String[A] = String(codec)
        override def imap[A, B](fa: Json.Primitive.String[A])(f: A => B)(g: B => A): Json.Primitive.String[B] =
          Json.Primitive.String(fa.value.imap(f)(g))

  final case class Record[A](value: Self.Record[Json.Key, Json, A]) extends Json[A]

  object Record:
    given RecordInvariant[Json.Record, Json.Field] with
      override def empty: Json.Record[Unit] = ???
      override def one[A](field: Field[A]): Json.Record[A] =
        Json.Record(Self.Record.Root(field, metadata = Metadata.Empty))

      override def imap[A, B](fa: Json.Record[A])(f: A => B)(g: B => A): Json.Record[B] =
        Json.Record(fa.value.imap(f)(g))

      extension [A](self: Json.Record[A])
        override def zip[B](codec: Json.Record[B]): Json.Record[(A, B)] =
          Record(self.value.zip(codec.value))

  final case class Tuple[A](value: Self.Tuple[Json, A]) extends Json[A]

  final case class Union[A](value: Self.Union[Json.Key, Json, A]) extends Json[A]

  type Key[A] = Json.Primitive.String[A]

  type Field[A] = Self.Field[Json.Key, Json, A]

  type Branch[A] = Self.Branch[Json.Key, Json, A]

  given FieldInvariant[Json.Field, Json.Record, Json.Key, Json] with
    override def apply[A, B](name: A, key: => Primitive.String[A], value: => Json[B]): Field[B] = Field.Required.Root(
      key = Reference.Constant(self = Reference.later(key), value = name),
      value = Reference.later(value),
      metadata = Metadata.Empty
    )

    override def imap[A, B](fa: Field[A])(f: A => B)(g: B => A): Field[B] = fa.imap(f)(g)

  given CodecInvariant.Nullable[Json, Json.Optional] with
    override def imap[A, B](fa: Json[A])(f: A => B)(g: B => A): Json[B] = fa match
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

object Playground:
  val string: Json.Primitive.String[String] = ???
  val int: Json.Primitive[Int] = ???
  val long: Json.Primitive[Long] = ???
  val field = summon[FieldInvariant[Json.Field, Json.Record, Json.Key, Json]]

  val rec1 = field("foo", string, string) :* field("bar", string, int) :* field("baz", string, long)
  val rec2 = field("foo", string, string) *: field("bar", string, int) *: field("baz", string, long)