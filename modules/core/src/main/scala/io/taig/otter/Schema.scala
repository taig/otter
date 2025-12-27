package io.taig.otter

import io.taig.otter as Self
import io.taig.otter.syntax.AllSyntax.*
import cats.Invariant
import cats.Contravariant
import cats.Functor
import io.taig.otter.operation.TupleableOperation
import io.taig.otter.operation.TupleOperation

sealed abstract class Schema[+S[a] <: Schema[?, a], A] extends Schema.Read[S, A], Schema.Write[S, A]:
  override def self: Annotation[Schema.Self[S, A]]

object Schema:
  type Self[+S[a] <: Schema[?, a], A] = Self.Collection[S, A] | Self.Tuple[S, A]

  def apply[S[a] <: Schema[?, a], A](self: Annotation[Schema.Self[S, A]]): Schema[S, A] = ???

  sealed trait Read[+S[a] <: Schema.Read[?, a], +A]:
    def self: Annotation[Schema.Read.Self[S, A]]

  object Read:
    type Self[+S[a] <: Schema.Read[?, a], +A] = Self.Collection.Read[S, A] | Self.Tuple.Read[S, A]

    def apply[S[a] <: Schema.Read[?, a], A](self: Annotation[Schema.Read.Self[S, A]]): Schema.Read[S, A] = ???

  sealed trait Write[+S[a] <: Schema.Write[?, a], -A]:
    def self: Annotation[Schema.Write.Self[S, A]]

  object Write:
    type Self[+S[a] <: Schema.Write[?, a], -A] = Self.Collection.Write[S, A] | Self.Tuple.Write[S, A]

    def apply[S[a] <: Schema.Write[?, a], A](self: Annotation[Schema.Write.Self[S, A]]): Schema.Write[S, A] = ???

  sealed abstract class Collection[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Collection.Read[S, A],
        Schema.Collection.Write[S, A]:
    override def self: Annotation[Self.Collection[S, A]]

  object Collection:
    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Self.Collection[S, A]]): Schema.Collection[S, A] =
      new Collection[S, A]:
        override def self: Self.Annotation[Self.Collection[S, A]] = annotation

    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[Self.Collection.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[Self.Collection.Read[S, A]]
      ): Schema.Collection.Read[S, A] = new Read[S, A]:
        override def self: Self.Annotation[Self.Collection.Read[S, A]] = annotation

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Self.Collection.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Self.Collection.Write[S, A]]
      ): Schema.Collection.Write[S, A] = new Write[S, A]:
        override def self: Self.Annotation[Self.Collection.Write[S, A]] = annotation

  sealed abstract class Primitive[A] extends Schema[Nothing, A], Schema.Primitive.Read[A], Schema.Primitive.Write[A]

  object Primitive:
    sealed trait Read[+A] extends Schema.Read[Nothing, A]

    sealed trait Write[-A] extends Schema.Write[Nothing, A]

    given Invariant[Schema.Primitive] = ???

  sealed abstract class Tuple[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Tuple.Read[S, A],
        Schema.Tuple.Write[S, A]:
    override def self: Annotation[Self.Tuple[S, A]]

  object Tuple:
    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Self.Tuple[S, A]]): Schema.Tuple[S, A] = new Tuple[S, A]:
      override def self: Self.Annotation[Self.Tuple[S, A]] = annotation

    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[Self.Tuple.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](annotation: Annotation[Self.Tuple.Read[S, A]]): Schema.Tuple.Read[S, A] =
        new Read[S, A]:
          override def self: Self.Annotation[Self.Tuple.Read[S, A]] = annotation

      given [S[a] <: Schema.Read[?, a]] => Functor[Schema.Tuple.Read[S, *]] =
        Functor[[a] =>> Annotation[Self.Tuple.Read[S, a]]].imapK([A] =>
          (self: Annotation[Self.Tuple.Read[S, A]]) => Schema.Tuple.Read[S, A](self)
        )([A] => (schema: Schema.Tuple.Read[S, A]) => schema.self)

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Self.Tuple.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Self.Tuple.Write[S, A]]
      ): Schema.Tuple.Write[S, A] = new Write[S, A]:
        override def self: Self.Annotation[Self.Tuple.Write[S, A]] = annotation

      given [S[a] <: Schema.Write[?, a]] => Contravariant[Schema.Tuple.Write[S, *]] =
        Contravariant[[a] =>> Annotation[Self.Tuple.Write[S, a]]].imapK([A] =>
          (self: Annotation[Self.Tuple.Write[S, A]]) => Schema.Tuple.Write[S, A](self)
        )([A] => (schema: Schema.Tuple.Write[S, A]) => schema.self)

    given [S[a] <: Schema[?, a]] => Invariant[Schema.Tuple[S, *]] =
      Invariant[[a] =>> Annotation[Self.Tuple[S, a]]].imapK([A] =>
        (self: Annotation[Self.Tuple[S, A]]) => Schema.Tuple[S, A](self)
      )([A] => (schema: Schema.Tuple[S, A]) => schema.self)

    given TupleOperation[
      Schema.Tuple,
      Schema.Tuple.Read,
      Schema.Tuple.Write,
      Schema[?, *],
      Schema.Read[?, *],
      Schema.Write[?, *]
    ] = ??? // TupleOperation[
    //   [S[a] <: Schema[?, a], A] =>> Annotation[Self.Tuple[S, A]],
    //   [S[a] <: Schema.Read[?, a], A] =>> Annotation[Self.Tuple.Read[S, A]],
    //   [S[a] <: Schema.Write[?, a], A] =>> Annotation[Self.Tuple.Write[S, A]],
    //   Schema,
    //   Schema.Read,
    //   Schema.Write,
    //   Schema[?, *],
    //   Schema.Read[?, *],
    //   Schema.Write[?, *]
    // ].imapK[Schema.Tuple, Schema.Tuple.Read, Schema.Tuple.Write](
    //   [S[a] <: Schema[?, a], A] => (annotation: Annotation[Self.Tuple[S, A]]) => Tuple(annotation),
    //   [S[a] <: Schema[?, a], A] => (schema: Schema.Tuple[S, A]) => schema.self
    // )(
    //   [S[a] <: Schema.Read[?, a], A] => (annotation: Annotation[Self.Tuple.Read[S, A]]) => Tuple.Read(annotation),
    //   [S[a] <: Schema.Read[?, a], A] => (schema: Schema.Tuple.Read[S, A]) => schema.self
    // )(
    //   [S[a] <: Schema.Write[?, a], A] => (annotation: Annotation[Self.Tuple.Write[S, A]]) => Tuple.Write(annotation),
    //   [S[a] <: Schema.Write[?, a], A] => (schema: Schema.Tuple.Write[S, A]) => schema.self
    // )

  given [S[a] <: Schema[?, a]] => Invariant[Schema[S, *]] = ???

  given [S[a] <: Schema[?, a] & R[a] & W[a], R[a] <: Schema.Read[?, a], W[a] <: Schema.Write[?, a]]
    => TupleableOperation[
      S,
      R,
      W,
      Schema.Tuple,
      Schema.Tuple.Read,
      Schema.Tuple.Write,
      Schema[?, *],
      Schema.Read[?, *],
      Schema.Write[?, *]
    ] = ??? // TupleableOperation.derived
