package io.taig.otter

import io.taig.otter as Self
import cats.Invariant

sealed abstract class Json[A]:
  def self: Collection[Json, A] | Constant[Json, A] | Dictionary[Json.Key, Json, A] | Enumeration[A] |
    Optional[Json, A] | Primitive[A] | Record[Json.Key, Json, A] | Tuple[Json, A] | Union[Json.Key, Json, A]

object Json:
  final case class Collection[A](self: Self.Collection[Json, A]) extends Json[A]
  final case class Constant[A](self: Self.Constant[Json, A]) extends Json[A]
  final case class Dictionary[A](self: Self.Dictionary[Json.Key, Json, A]) extends Json[A]
  final case class Enumeration[A](self: Self.Enumeration[A]) extends Json[A]
  final case class Optional[A](self: Self.Optional[Json, A]) extends Json[A]
  final case class Primitive[A](self: Self.Primitive[A]) extends Json[A]
  final case class Record[A](self: Self.Record[Json.Key, Json, A]) extends Json[A]
  final case class Tuple[A](self: Self.Tuple[Json, A]) extends Json[A]
  final case class Union[A](self: Self.Union[Json.Key, Json, A]) extends Json[A]

  type Key[A] = Self.Primitive.String[A]

  given Invariant[Json] = new Invariant[Json]:
    override def imap[A, B](fa: Json[A])(f: A => B)(g: B => A): Json[B] = ???
    // fa.self match
    //   case codec: Collection[Json, A]           => codec.imap(f)(g)
    //   case codec: Constant[Json, A]             => codec.imap(f)(g)
    //   case codec: Dictionary[Json.Key, Json, A] => codec.imap(f)(g)
    //   case codec: Enumeration[A]                => codec.imap(f)(g)
    //   case codec: Optional[Json, A]             => codec.imap(f)(g)
    //   case codec: Primitive[A]                  => codec.imap(f)(g)
    //   case codec: Record[Json.Key, Json, A]     => codec.imap(f)(g)
    //   case codec: Tuple[Json, A]                => codec.imap(f)(g)
    //   case codec: Union[Json.Key, Json, A]      => codec.imap(f)(g)
