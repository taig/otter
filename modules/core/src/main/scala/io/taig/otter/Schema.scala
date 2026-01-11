package io.taig.otter

import io.taig.otter as Self
import io.taig.otter.syntax.all.*
import cats.syntax.all.*
import cats.Functor
import cats.Contravariant
import cats.Invariant
import cats.Apply
import cats.ContravariantSemigroupal
import Self.operation.TupleOperation
import cats.InvariantSemigroupal

sealed abstract class Schema[A] extends Schema.Read[A], Schema.Write[A]:
  override type Of[a] <: Schema[a]

  override def self: Annotation[Schema.Self[Of, A]]

object Schema:
  sealed trait Read[+A]:
    type Of[a] <: Schema.Read[a]

    def self: Annotation[Schema.Read.Self[Of, A]]

  object Read:
    type Self[+S[a] <: Schema.Read[a], +A] = Self.Tuple.Read[S, A]

    type Of[+S[a] <: Schema.Read[a], +A] = Schema.Read[A] { type Of[a] <: S[a] }

    given Functor[Schema.Read]:
      override def map[A, B](schema: Schema.Read[A])(f: A => B): Schema.Read[B] = schema match
        case schema: Schema.Read[A] => schema.map(f)

  sealed trait Write[-A]:
    type Of[a] <: Schema.Write[a]

    def self: Annotation[Schema.Write.Self[Of, A]]

  object Write:
    type Self[+S[a] <: Schema.Write[a], -A] = Self.Tuple.Write[S, A]

    type Of[+S[a] <: Schema.Write[a], -A] = Schema.Write[A] { type Of[a] <: S[a] }

    given Contravariant[Schema.Write]:
      override def contramap[A, B](schema: Schema.Write[A])(f: B => A): Schema.Write[B] = schema match
        case schema: Schema.Write[A] => schema.contramap(f)

  sealed abstract class Tuple[A] extends Schema[A], Schema.Tuple.Read[A], Schema.Tuple.Write[A]:
    override type Of[a] <: Schema[a]

    override def self: Annotation[Self.Tuple[Of, A]]

  object Tuple:
    sealed trait Read[+A] extends Schema.Read[A]:
      override type Of[a] <: Schema.Read[a]

      override def self: Annotation[Self.Tuple.Read[Of, A]]

    object Read:
      type Of[+S[a] <: Schema.Read[a], +A] = Schema.Tuple.Read[A] { type Of[a] <: S[a] }

      def apply[S[a] <: Schema.Read[a], A](annotation: Annotation[Self.Tuple.Read[S, A]]): Schema.Tuple.Read.Of[S, A] =
        new Schema.Tuple.Read[A]:
          override type Of[a] = S[a]
          override def self: Annotation[Self.Tuple.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[a], A](
          schema: Schema.Tuple.Read.Of[S, A]
      ): Annotation[Self.Tuple.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[a]] => Apply[Schema.Tuple.Read.Of[S, *]] =
        Apply[[a] =>> Annotation[Self.Tuple.Read[S, a]]].imapK([A] =>
          (self: Annotation[Self.Tuple.Read[S, A]]) => Read(self)
        )([A] => (schema: Schema.Tuple.Read.Of[S, A]) => schema.self)

      given TupleOperation[Schema.Tuple.Read.Of, Schema.Read] = ???

    sealed trait Write[-A] extends Schema.Write[A]:
      override type Of[a] <: Schema.Write[a]

      override def self: Annotation[Self.Tuple.Write[Of, A]]

    object Write:
      type Of[+S[a] <: Schema.Write[a], -A] = Schema.Tuple.Write[A] { type Of[a] <: S[a] }

      def apply[S[a] <: Schema.Write[a], A](
          annotation: Annotation[Self.Tuple.Write[S, A]]
      ): Schema.Tuple.Write.Of[S, A] =
        new Schema.Tuple.Write[A]:
          override type Of[a] = S[a]
          override def self: Annotation[Self.Tuple.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[a], A](
          schema: Schema.Tuple.Write.Of[S, A]
      ): Annotation[Self.Tuple.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[a]] => ContravariantSemigroupal[Schema.Tuple.Write.Of[S, *]] =
        ContravariantSemigroupal[[a] =>> Annotation[Self.Tuple.Write[S, a]]].imapK([A] =>
          (self: Annotation[Self.Tuple.Write[S, A]]) => Write(self)
        )([A] => (schema: Schema.Tuple.Write.Of[S, A]) => schema.self)

      given TupleOperation[Schema.Tuple.Write.Of, Schema.Write] = ???

    type Of[+S[a] <: Schema[a], A] = Schema.Tuple[A] { type Of[a] <: S[a] }

    def apply[S[a] <: Schema[a], A](annotation: Annotation[Self.Tuple[S, A]]): Schema.Tuple.Of[S, A] =
      new Schema.Tuple[A]:
        override type Of[a] = S[a]
        override def self: Annotation[Self.Tuple[S, A]] = annotation

    def unapply[S[a] <: Schema[a], A](schema: Schema.Tuple.Of[S, A]): Annotation[Self.Tuple[S, A]] = schema.self

    given [S[a] <: Schema[a]] => InvariantSemigroupal[Schema.Tuple.Of[S, *]] =
      InvariantSemigroupal[[a] =>> Annotation[Self.Tuple[S, a]]].imapK([A] =>
        (self: Annotation[Self.Tuple[S, A]]) => Schema.Tuple(self)
      )([A] => (schema: Schema.Tuple.Of[S, A]) => schema.self)

    given TupleOperation[Schema.Tuple.Of, Schema] = ???

  type Self[+S[a] <: Schema[a], A] = Self.Tuple[S, A]

  type Of[+S[a] <: Schema[a], A] = Schema[A] { type Of[a] <: S[a] }

  given Invariant[Schema]:
    override def imap[A, B](schema: Schema[A])(f: A => B)(g: B => A): Schema[B] = schema match
      case schema: Schema[A] => schema.imap(f)(g)
