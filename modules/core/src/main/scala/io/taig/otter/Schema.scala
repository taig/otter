package io.taig.otter

import io.taig.otter.syntax.InvariantSyntax.*
import io.taig.otter as Self
import cats.Invariant
import io.taig.otter.operation.CollectionOperation
import io.taig.validation.Validation
import cats.data.Chain
import io.taig.otter.component.CollectionComponent

sealed abstract class Schema[+S[_], A](using S :<: Schema[?, *]) extends Schema.Read[S, A], Schema.Write[S, A]:
  override def self: Annotation[Self.Collection[S, A] | Self.Primitive[A]]

object Schema:
  sealed trait Read[+S[_], +A](using S :<: Schema[?, *]):
    def self: Annotation[Self.Collection.Read[S, A] | Self.Primitive.Read[A]]

  sealed trait Write[+S[_], -A](using S :<: Schema[?, *]):
    def self: Annotation[Self.Collection.Write[S, A] | Self.Primitive.Write[A]]

  sealed abstract class Collection[+S[_], A](using S :<: Schema[?, *])
      extends Schema[S, A],
        Collection.Read[S, A],
        Collection.Write[S, A]:
    override def self: Annotation[Self.Collection[S, A]]

  object Collection:
    sealed trait Read[+S[_], +A](using S :<: Schema[?, *]) extends Schema.Read[S, A]:
      override def self: Annotation[Self.Collection.Read[S, A]]

    sealed trait Write[+S[_], -A](using S :<: Schema[?, *]) extends Schema.Write[S, A]:
      override def self: Annotation[Self.Collection.Write[S, A]]

    def apply[S[_], A](annotation: Annotation[Self.Collection[S, A]])(using S :<: Schema[?, *]): Collection[S, A] =
      new Collection[S, A]:
        override def self: Annotation[Self.Collection[S, A]] = annotation

    given [S[_]](using S :<: Schema[?, *]): Invariant[Collection[S, *]] =
      Invariant[[a] =>> Annotation[Self.Collection[S, a]]].imapK([A] =>
        (annotation: Annotation[Self.Collection[S, A]]) => Collection(annotation)
      )([A] => (schema: Collection[S, A]) => schema.self)

    given CollectionOperation[Schema.Collection, Schema[?, *]] =
      CollectionOperation[[s[_], a] =>> Annotation[Self.Collection[s, a]], Schema[?, *]]

      new CollectionOperation[Schema.Collection, Schema[?, *]]:
        override def chained[Value[_], A](
            schema: Reference[Value, A],
            validation: Validation[io.taig.validation.Constraint.Collection, Chain[A]]
        )(using Value :<: Schema[?, *]): Collection[Value, Chain[A]] =
          Collection(Annotation(Self.Collection.Chained(schema, validation)))

  sealed abstract class Primitive[A] extends Schema[Nothing, A], Schema.Primitive.Read[A], Schema.Primitive.Write[A]:
    override def self: Annotation[Self.Primitive[A]]

  object Primitive:
    sealed trait Read[+A] extends Schema.Read[Nothing, A]:
      override def self: Annotation[Self.Primitive.Read[A]]

    object Read:
      sealed trait Boolean[+A] extends Schema.Primitive.Read[A]:
        override def self: Annotation[Self.Primitive.Boolean.Read[A]]

    sealed trait Write[-A] extends Schema.Write[Nothing, A]:
      override def self: Annotation[Self.Primitive.Write[A]]

    sealed abstract class Boolean[A] extends Schema.Primitive[A]:
      override def self: Annotation[Self.Primitive.Boolean[A]]

object SchemaComponent extends CollectionComponent[Schema.Collection, Schema[?, *]]
