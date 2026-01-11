package io.taig.otter

import io.taig.otter as Self
import io.taig.otter.syntax.all.*
import cats.syntax.all.*
import cats.Invariant
import cats.Contravariant
import cats.Functor
import io.taig.otter.operation.TupleableOperation
import io.taig.otter.operation.TupleOperation
import cats.InvariantSemigroupal
import cats.Apply
import cats.ContravariantSemigroupal

sealed abstract class Json[A] extends Json.Read[A], Json.Write[A]:
  override def self: Annotation[Json.Of[A]]

object Json:
  sealed trait Read[+A]:
    def self: Annotation[Json.Read.Of[A]]

  object Read:
    type Of[+A] = Self.Primitive.Read[A] | Self.Tuple.Read[Json.Read, A]

    given Functor[Json.Read]:
      override def map[A, B](json: Json.Read[A])(f: A => B): Read[B] = json match
        case json: Json.Primitive.Read[A] => json.map(f)
        case json: Json.Tuple.Read[A]     => ??? // json.map(f)

    given TupleableOperation[Json.Tuple.Read, Json.Read] = TupleableOperation.derived

  sealed trait Write[-A]:
    def self: Annotation[Json.Write.Of[A]]

  object Write:
    type Of[-A] = Self.Primitive.Write[A] | Self.Tuple.Write[Json.Write, A]

    given Contravariant[Json.Write]:
      override def contramap[A, B](json: Json.Write[A])(f: B => A): Write[B] = json match
        case json: Json.Primitive.Write[A] => json.contramap(f)
        case json: Json.Tuple.Write[A]     => ??? // json.contramap(f)

    given TupleableOperation[Json.Tuple.Write, Json.Write] = TupleableOperation.derived

  sealed abstract class Primitive[A] extends Json[A], Json.Primitive.Read[A], Json.Primitive.Write[A]:
    def self: Annotation[Self.Primitive[A]]

  object Primitive:
    sealed trait Read[+A] extends Json.Read[A]:
      def self: Annotation[Self.Primitive.Read[A]]

    object Read:
      def apply[A](annotation: Annotation[Self.Primitive.Read[A]]): Json.Primitive.Read[A] = new Read[A]:
        override def self: Annotation[Self.Primitive.Read[A]] = annotation

      def unapply[A](json: Json.Primitive.Read[A]): Annotation[Self.Primitive.Read[A]] = json.self

      given Functor[Json.Primitive.Read] = Functor[[a] =>> Annotation[Self.Primitive.Read[a]]]
        .imapK([A] => (self: Annotation[Self.Primitive.Read[A]]) => Read(self))([A] =>
          (json: Json.Primitive.Read[A]) => json.self
        )

    sealed trait Write[-A] extends Json.Write[A]:
      def self: Annotation[Self.Primitive.Write[A]]

    object Write:
      def apply[A](annotation: Annotation[Self.Primitive.Write[A]]): Json.Primitive.Write[A] = new Write[A]:
        override def self: Annotation[Self.Primitive.Write[A]] = annotation

      def unapply[A](json: Json.Primitive.Write[A]): Annotation[Self.Primitive.Write[A]] = json.self

      given Contravariant[Json.Primitive.Write] = Contravariant[[a] =>> Annotation[Self.Primitive.Write[a]]]
        .imapK([A] => (self: Annotation[Self.Primitive.Write[A]]) => Write(self))([A] =>
          (json: Json.Primitive.Write[A]) => json.self
        )

    def apply[A](annotation: Annotation[Self.Primitive[A]]): Json.Primitive[A] = new Primitive[A]:
      override def self: Annotation[Self.Primitive[A]] = annotation

    def unapply[A](json: Json.Primitive[A]): Annotation[Self.Primitive[A]] = json.self

    given Invariant[Json.Primitive] = Invariant[[a] =>> Annotation[Self.Primitive[a]]]
      .imapK([A] => (self: Annotation[Self.Primitive[A]]) => Primitive(self))([A] =>
        (json: Json.Primitive[A]) => json.self
      )

  sealed abstract class Tuple[A] extends Json[A], Json.Tuple.Read[A], Json.Tuple.Write[A]:
    def self: Annotation[Self.Tuple[Json, A]]

  object Tuple:
    sealed trait Read[+A] extends Json.Read[A]:
      def self: Annotation[Self.Tuple.Read[Json.Read, A]]

    object Read:
      def apply[A](annotation: Annotation[Self.Tuple.Read[Json.Read, A]]): Json.Tuple.Read[A] =
        new Json.Tuple.Read[A]:
          override def self: Annotation[Self.Tuple.Read[Json.Read, A]] = annotation

      def unapply[A](json: Json.Tuple.Read[A]): Annotation[Self.Tuple.Read[Json.Read, A]] = json.self

      given Apply[Json.Tuple.Read] = Apply[[a] =>> Annotation[Self.Tuple.Read[Json.Read, a]]]
        .imapK([A] => (self: Annotation[Self.Tuple.Read[Json.Read, A]]) => Read(self))([A] =>
          (json: Json.Tuple.Read[A]) => json.self
        )

      given TupleOperation[Json.Tuple.Read, Json.Read] = ???

    sealed trait Write[-A] extends Json.Write[A]:
      def self: Annotation[Self.Tuple.Write[Json.Write, A]]

    object Write:
      def apply[A](annotation: Annotation[Self.Tuple.Write[Json.Write, A]]): Json.Tuple.Write[A] =
        new Json.Tuple.Write[A]:
          override def self: Annotation[Self.Tuple.Write[Json.Write, A]] = annotation

      def unapply[A](json: Json.Tuple.Write[A]): Annotation[Self.Tuple.Write[Json.Write, A]] = json.self

      given ContravariantSemigroupal[Json.Tuple.Write] =
        ContravariantSemigroupal[[a] =>> Annotation[Self.Tuple.Write[Json.Write, a]]]
          .imapK([A] => (self: Annotation[Self.Tuple.Write[Json.Write, A]]) => Write(self))([A] =>
            (json: Json.Tuple.Write[A]) => json.self
          )

      given TupleOperation[Json.Tuple.Write, Json.Write] = ???

    def apply[A](annotation: Annotation[Self.Tuple[Json, A]]): Json.Tuple[A] = new Tuple[A]:
      override def self: Annotation[Self.Tuple[Json, A]] = annotation

    def unapply[A](json: Json.Tuple[A]): Annotation[Self.Tuple[Json, A]] = json.self

    given InvariantSemigroupal[Json.Tuple] = InvariantSemigroupal[[a] =>> Annotation[Self.Tuple[Json, a]]]
      .imapK([A] => (self: Annotation[Self.Tuple[Json, A]]) => Tuple(self))([A] => (json: Json.Tuple[A]) => json.self)

    given TupleOperation[Json.Tuple, Json] = ???

  type Of[A] = Self.Primitive[A] | Self.Tuple[Json, A]

  given Invariant[Json]:
    override def imap[A, B](json: Json[A])(f: A => B)(g: B => A): Json[B] = json match
      case json: Json.Primitive[A] => json.imap(f)(g)
      case json: Json.Tuple[A]     => ??? // json.imap(f)(g)

  given TupleableOperation[Json.Tuple, Json] = TupleableOperation.derived
