package io.taig.otter

import io.taig.otter as Self
import io.taig.otter.syntax.AllSyntax.*
import io.taig.otter.operation.TupleOperation
import cats.Invariant

sealed abstract class Schema[A] extends Schema.Read[A], Schema.Write[A]:
  override type Of[a] <: Schema[a]

  override def self: Annotation[Schema.Self[Of, A]]

object Schema:
  type Self[+S[a] <: Schema[a], A] = Self.Collection[S, A] | Self.Tuple[S, A]

  type Of[+S[a] <: Schema[a], A] = Schema[A] { type Of[a] <: S[a] }

  def apply[S[a] <: Schema[a], A](self: Annotation[Schema.Self[S, A]]): Schema.Of[S, A] = ???

  sealed trait Read[+A]:
    type Of[a] <: Schema.Read[a]

    def self: Annotation[Schema.Read.Self[Of, A]]

  object Read:
    type Self[+S[a] <: Schema.Read[a], +A] = Self.Collection.Read[S, A] | Self.Tuple.Read[S, A]

    type Of[+S[a] <: Schema.Read[a], +A] = Schema.Read[A] { type Of[a] <: S[a] }

    def apply[S[a] <: Schema.Read[a], A](self: Annotation[Schema.Read.Self[S, A]]): Schema.Read.Of[S, A] = ???

  sealed trait Write[-A]:
    type Of[a] <: Schema.Write[a]

    def self: Annotation[Schema.Write.Self[Of, A]]

  object Write:
    type Self[+S[a] <: Schema.Write[a], -A] = Self.Collection.Write[S, A] | Self.Tuple.Write[S, A]

    type Of[+S[a] <: Schema.Write[a], -A] = Schema.Write[A] { type Of[a] <: S[a] }

    def apply[S[a] <: Schema.Write[a], A](self: Annotation[Schema.Write.Self[S, A]]): Schema.Write.Of[S, A] = ???

  sealed abstract class Collection[A] extends Schema[A], Schema.Collection.Read[A], Schema.Collection.Write[A]:
    override type Of[a] <: Schema[a]

    override def self: Annotation[Self.Collection[Of, A]]

  object Collection:
    type Of[+S[a] <: Schema[a], A] = Schema.Collection[A] { type Of[a] <: S[a] }

    def apply[S[a] <: Schema[a], A](annotation: Annotation[Self.Collection[S, A]]): Schema.Collection.Of[S, A] =
      new Collection[A]:
        override type Of[a] = S[a]

        override def self: Self.Annotation[Self.Collection[S, A]] = annotation

    sealed trait Read[+A] extends Schema.Read[A]:
      override type Of[a] <: Schema.Read[a]

      override def self: Annotation[Self.Collection.Read[Of, A]]

    object Read:
      type Of[+S[a] <: Schema.Read[a], +A] = Schema.Collection.Read[A] { type Of[a] <: S[a] }

      def apply[S[a] <: Schema.Read[a], A](annotation: Annotation[Self.Collection.Read[S, A]]): Schema.Read.Of[S, A] =
        new Read[A]:
          override type Of[a] = S[a]

          override def self: Self.Annotation[Self.Collection.Read[S, A]] = annotation

    sealed trait Write[-A] extends Schema.Write[A]:
      override type Of[a] <: Schema.Write[a]

      override def self: Annotation[Self.Collection.Write[Of, A]]

    object Write:
      type Of[+S[a] <: Schema.Write[a], -A] = Schema.Collection.Write[A] { type Of[a] <: S[a] }

      def apply[S[a] <: Schema.Write[a], A](
          annotation: Annotation[Self.Collection.Write[S, A]]
      ): Schema.Write.Of[S, A] =
        new Write[A]:
          override type Of[a] = S[a]

          override def self: Self.Annotation[Self.Collection.Write[S, A]] = annotation

  sealed abstract class Primitive[A] extends Schema[A], Schema.Primitive.Read[A], Schema.Primitive.Write[A]:
    override type Of[a] = Nothing

  object Primitive:
    sealed trait Read[+A] extends Schema.Read[A]:
      override type Of[a] = Nothing

    sealed trait Write[-A] extends Schema.Write[A]:
      override type Of[a] = Nothing

  sealed abstract class Tuple[A] extends Schema[A], Schema.Tuple.Read[A], Schema.Tuple.Write[A]:
    override type Of[a] <: Schema[a]

    override def self: Annotation[Self.Tuple[Of, A]]

  object Tuple:
    type Of[+S[a] <: Schema[a], A] = Schema.Tuple[A] { type Of[a] <: S[a] }

    def apply[S[a] <: Schema[a], A](annotation: Annotation[Self.Tuple[S, A]]): Schema.Tuple.Of[S, A] = new Tuple[A]:
      override type Of[a] = S[a]

      override def self: Self.Annotation[Self.Tuple[S, A]] = annotation

    sealed trait Read[+A] extends Schema.Read[A]:
      override type Of[a] <: Schema.Read[a]

      override def self: Annotation[Self.Tuple.Read[Of, A]]

    object Read:
      type Of[+S[a] <: Schema.Read[a], +A] = Schema.Tuple.Read[A] { type Of[a] <: S[a] }

      def apply[S[a] <: Schema.Read[a], A](annotation: Annotation[Self.Tuple.Read[S, A]]): Schema.Tuple.Read.Of[S, A] =
        new Read[A]:
          override type Of[a] = S[a]

          override def self: Self.Annotation[Self.Tuple.Read[S, A]] = annotation

    sealed trait Write[-A] extends Schema.Write[A]:
      override type Of[a] <: Schema.Write[a]

      override def self: Annotation[Self.Tuple.Write[Of, A]]

    object Write:
      type Of[+S[a] <: Schema.Write[a], -A] = Schema.Tuple.Write[A] { type Of[a] <: S[a] }

      def apply[S[a] <: Schema.Write[a], A](
          annotation: Annotation[Self.Tuple.Write[S, A]]
      ): Schema.Tuple.Write.Of[S, A] =
        new Write[A]:
          override type Of[a] = S[a]

          override def self: Self.Annotation[Self.Tuple.Write[S, A]] = annotation

    given [S[a] <: Schema[a]] => Invariant[Schema.Tuple.Of[S, *]] =
      Invariant[[a] =>> Annotation[Self.Tuple[S, a]]].imapK([A] =>
        (self: Annotation[Self.Tuple[S, A]]) => Schema.Tuple[S, A](self)
      )([A] => (schema: Schema.Tuple.Of[S, A]) => schema.self)

    given TupleOperation[
      Schema.Tuple.Of,
      Schema.Tuple.Read.Of,
      Schema.Tuple.Write.Of,
      Schema.Of,
      Schema.Read.Of,
      Schema.Write.Of,
      Schema,
      Schema.Read,
      Schema.Write
    ] = TupleOperation[
      [S[a] <: Schema[a], A] =>> Annotation[Self.Tuple[S, A]],
      [S[a] <: Schema.Read[a], A] =>> Annotation[Self.Tuple.Read[S, A]],
      [S[a] <: Schema.Write[a], A] =>> Annotation[Self.Tuple.Write[S, A]],
      Schema.Of,
      Schema.Read.Of,
      Schema.Write.Of,
      Schema,
      Schema.Read,
      Schema.Write
    ].imapK[Schema.Tuple.Of, Schema.Tuple.Read.Of, Schema.Tuple.Write.Of](
      [S[a] <: Schema[a], A] => (annotation: Annotation[Self.Tuple[S, A]]) => Tuple(annotation),
      [S[a] <: Schema[a], A] => (schema: Schema.Tuple.Of[S, A]) => schema.self
    )(
      [S[a] <: Schema.Read[a], A] => (annotation: Annotation[Self.Tuple.Read[S, A]]) => Tuple.Read(annotation),
      [S[a] <: Schema.Read[a], A] => (schema: Schema.Tuple.Read.Of[S, A]) => schema.self
    )(
      [S[a] <: Schema.Write[a], A] => (annotation: Annotation[Self.Tuple.Write[S, A]]) => Tuple.Write(annotation),
      [S[a] <: Schema.Write[a], A] => (schema: Schema.Tuple.Write.Of[S, A]) => schema.self
    )

object Playground:
  val a: Schema.Tuple.Of[Schema.Primitive, String] = ???
  val b: Schema[String] = ???
  val c: Schema.Primitive[Int] = ???

  val x: Schema.Tuple.Of[Schema, (String, Int)] = a :* c
