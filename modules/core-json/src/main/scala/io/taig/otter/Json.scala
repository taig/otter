package io.taig.otter

import io.taig.otter as Self
import cats.Invariant

sealed abstract class Json[A]:
  def self: Self.Collection[Json, A] | Self.Constant[Json, A] | Self.Dictionary[Json.Key, Json, A] |
    Self.Enumeration[A] | Self.Optional[Json, A] | Self.Primitive[A] | Self.Record[Json.Key, Json, A] |
    Self.Tuple[Json, A] | Self.Union[Json.Key, Json, A]

object Json:
  final case class Collection[A](self: Self.Collection[Json, A]) extends Json[A]
  final case class Constant[A](self: Self.Constant[Json, A]) extends Json[A]

  final case class Dictionary[A](self: Self.Dictionary[Json.Key, Json, A]) extends Json[A]

  final case class Enumeration[A](self: Self.Enumeration[A]) extends Json[A]

  final case class Optional[A](self: Self.Optional[Json, A]) extends Json[A]

  sealed abstract class Primitive[A] extends Json[A]

  object Primitive:
    final case class Boolean[A](self: Self.Primitive.Boolean[A]) extends Json.Primitive[A]

    object Boolean:
      given Invariant[Json.Primitive.Boolean] with
        override def imap[A, B](fa: Json.Primitive.Boolean[A])(f: A => B)(g: B => A): Json.Primitive.Boolean[B] =
          Json.Primitive.Boolean(fa.self.imap(f)(g))

    final case class Number[A](self: Self.Primitive.Number[A]) extends Json.Primitive[A]

    object Number:
      given Invariant[Json.Primitive.Number] with
        override def imap[A, B](fa: Json.Primitive.Number[A])(f: A => B)(g: B => A): Json.Primitive.Number[B] =
          Json.Primitive.Number(fa.self.imap(f)(g))

    final case class String[A](self: Self.Primitive.String[A]) extends Json.Primitive[A]

    object String:
      given Invariant[Json.Primitive.String] with
        override def imap[A, B](fa: Json.Primitive.String[A])(f: A => B)(g: B => A): Json.Primitive.String[B] =
          Json.Primitive.String(fa.self.imap(f)(g))

  final case class Record[A](self: Self.Record[Json.Key, Json, A]) extends Json[A]

  final case class Tuple[A](self: Self.Tuple[Json, A]) extends Json[A]

  final case class Union[A](self: Self.Union[Json.Key, Json, A]) extends Json[A]

  type Key[A] = Json.Primitive.String[A]
