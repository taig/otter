package io.taig.otter

import io.taig.otter as Self
import cats.Invariant
import cats.derived.*

sealed abstract class Json[A] extends Product with Serializable derives Invariant

object Json:
  final case class Collection[A](self: Self.Collection[Json, A]) extends Json[A] derives Invariant
  final case class Constant[A](self: Self.Constant[Json, A]) extends Json[A] derives Invariant
  final case class Dictionary[A](self: Self.Dictionary[Json.Key, Json, A]) extends Json[A] derives Invariant
  final case class Enumeration[A](self: Self.Enumeration[Json.Primitive, A]) extends Json[A] derives Invariant
  final case class Optional[A](self: Self.Optional[Json, A]) extends Json[A] derives Invariant
  final case class Primitive[A](self: Self.Primitive[A]) extends Json[A] derives Invariant
  final case class Record[A](self: Self.Record[Json.Key, Json, A]) extends Json[A] derives Invariant
  final case class Tuple[A](self: Self.Tuple[Json, A]) extends Json[A] derives Invariant
  final case class Union[A](self: Self.Union[Json, A]) extends Json[A] derives Invariant

  sealed abstract class Key[A] extends Product with Serializable derives Invariant

  object Key:
    final case class Constant[A](self: Self.Constant[Json.Key.Primitive, A]) extends Json.Key[A] derives Invariant
    final case class Enumeration[A](self: Self.Enumeration[Json.Key.Primitive, A]) extends Json.Key[A] derives Invariant
    final case class Primitive[A](self: Self.Primitive.String[A]) extends Json.Key[A] derives Invariant
    final case class Union[A](self: Self.Union.Untagged[Json.Key, A]) extends Json.Key[A] derives Invariant
