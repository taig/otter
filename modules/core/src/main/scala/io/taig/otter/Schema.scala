package io.taig.otter

import io.taig.otter as Self
import io.taig.otter.operation.CollectionOperation
import io.taig.otter.syntax.CatsSyntax.*
import cats.Functor
import cats.Contravariant

sealed abstract class Schema[+S[a] <: Schema[?, a], A] extends Schema.Read[S, A], Schema.Write[S, A]:
  override def self: Annotation[Self.Collection[S, A]]

object Schema:
  sealed trait Read[+S[a] <: Schema.Read[?, a], A]:
    def self: Annotation[Self.Collection.Read[S, A]]

  object Read:
    def apply[S[a] <: Schema[?, a], A](
        annotation: Annotation[Self.Collection.Read[S, A]]
    ): Schema.Read[S, A] = new Read[S, A]:
      override def self: Annotation[Self.Collection.Read[S, A]] = annotation

  sealed trait Write[+S[a] <: Schema.Write[?, a], -A]:
    def self: Annotation[Self.Collection.Write[S, A]]

  object Write:
    def unapply[S[a] <: Schema.Write[?, a], A](
        schema: Schema.Collection.Write[S, A]
    ): Annotation[Self.Collection.Write[S, A]] = schema.self

  sealed abstract class Collection[+S[a] <: Schema[?, a], A] extends Collection.Read[S, A], Collection.Write[S, A]:
    override def self: Annotation[Self.Collection[S, A]]

  object Collection:
    sealed trait Read[+S[a] <: Schema.Read[?, a], A] extends Schema.Read[S, A]:
      override def self: Annotation[Self.Collection.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[Self.Collection.Read[S, A]]
      ): Schema.Collection.Read[S, A] = new Read[S, A]:
        override def self: Annotation[Self.Collection.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Collection.Read[S, A]
      ): Annotation[Self.Collection.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Collection.Read[S, *]] =
        Functor[[a] =>> Annotation[Self.Collection.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[Self.Collection.Read[S, A]]) => Read(annotation)
        )([A] => (schema: Schema.Collection.Read[S, A]) => schema.self)

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Self.Collection.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Self.Collection.Write[S, A]]
      ): Schema.Collection.Write[S, A] = new Write[S, A]:
        override def self: Annotation[Self.Collection.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Collection.Write[S, A]
      ): Annotation[Self.Collection.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Collection.Write[S, *]] =
        Contravariant[[a] =>> Annotation[Self.Collection.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[Self.Collection.Write[S, A]]) => Write(annotation)
        )([A] => (schema: Schema.Collection.Write[S, A]) => schema.self)

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Self.Collection[S, A]]): Schema.Collection[S, A] =
      new Collection[S, A]:
        override def self: Annotation[Self.Collection[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Collection[S, A]): Annotation[Self.Collection[S, A]] =
      schema.self

    given CollectionOperation[Schema.Collection, Schema[?, *]] =
      CollectionOperation[[s[a] <: Schema[?, a], a] =>> Annotation[Self.Collection[s, a]], Schema[?, *]]
        .imapK[Schema.Collection]([s[a] <: Schema[?, a], a] =>
          (self: Annotation[Self.Collection[s, a]]) => Collection(self)
        )([s[a] <: Schema[?, a], a] => (schema: Schema.Collection[s, a]) => schema.self)

  def unapply[S[a] <: Schema[?, a], A](schema: Schema[S, A]): Annotation[Self.Collection[S, A]] =
    schema.self
