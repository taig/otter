package io.taig.otter.operation

import io.taig.otter.Reference
import io.taig.otter.InvariantK
import cats.data.Chain
import io.taig.validation.Validation
import io.taig.otter.Constraint
import io.taig.otter.Constraint.Collection

trait CollectionOperation[F[+_[a] <: G[a], _], G[_]]:
  def chained[H[a] <: G[a], A](
      schema: Reference[H, A],
      validation: Validation[Constraint.Collection, Chain[A]]
  ): F[H, Chain[A]]

  def linked[H[a] <: G[a], A](
      schema: Reference[H, A],
      validation: Validation[Constraint.Collection, List[A]]
  ): F[H, List[A]]

  def indexed[H[a] <: G[a], A](
      schema: Reference[H, A],
      validation: Validation[Constraint.Collection, Vector[A]]
  ): F[H, Vector[A]]

  def schema[H[a] <: G[a], A](self: F[G, A]): Reference[G, ?]

object CollectionOperation:
  inline def apply[F[+_[a] <: G[a], _], G[_]](using self: CollectionOperation[F, G]): CollectionOperation[F, G] = self

  given InvariantK[CollectionOperation] with
    extension [H[+_[a] <: G[a], _], G[_]](fa: CollectionOperation[H, G])
      override def imapK[I[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => H[S, A] => I[S, A])(
          gK: [S[a] <: G[a], A] => I[S, A] => H[S, A]
      ): CollectionOperation[I, G] = new CollectionOperation[I, G]:
        override def chained[H[a] <: G[a], A](
            schema: Reference[H, A],
            validation: Validation[Collection, Chain[A]]
        ): I[H, Chain[A]] = fK(fa.chained(schema, validation))

        override def linked[H[a] <: G[a], A](
            schema: Reference[H, A],
            validation: Validation[Constraint.Collection, List[A]]
        ): I[H, List[A]] = fK(fa.linked(schema, validation))

        override def indexed[H[a] <: G[a], A](
            schema: Reference[H, A],
            validation: Validation[Constraint.Collection, Vector[A]]
        ): I[H, Vector[A]] = fK(fa.indexed(schema, validation))

        override def schema[H[a] <: G[a], A](iga: I[G, A]): Reference[G, ?] = fa.schema(gK(iga))
