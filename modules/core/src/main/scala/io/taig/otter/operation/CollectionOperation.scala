package io.taig.otter.operation

import cats.data.Chain
import io.taig.otter.Reference
import io.taig.otter.:<:
import io.taig.validation.Constraint
import io.taig.validation.Validation
import io.taig.otter.InvariantKA
import io.taig.validation.Constraint.Collection

trait CollectionOperation[F[_[_], _], G[_]]:
  self =>

  def chained[Value[_], A](
      schema: Reference[Value, A],
      validation: Validation[Constraint.Collection, Chain[A]]
  )(using Value :<: G): F[Value, Chain[A]]

  def imapKA[H[_[_], _]](fKA: [S[_], A] => F[S, A] => H[S, A])(
      gKA: [S[_], A] => H[S, A] => F[S, A]
  ): CollectionOperation[H, G] = new CollectionOperation[H, G]:
    override def chained[Value[_], A](
        schema: Reference[Value, A],
        validation: Validation[Collection, Chain[A]]
    )(using Value :<: G): H[Value, Chain[A]] = fKA(self.chained(schema, validation))

object CollectionOperation:
  inline def apply[F[_[_], _], G[_]](using self: CollectionOperation[F, G]): CollectionOperation[F, G] = self

  given [F[_]]: InvariantKA[[f[_[_], _]] =>> CollectionOperation[f, F]] with
    extension [G[_[_], _]](self: CollectionOperation[G, F])
      override def imapKA[H[_[_], _]](fKA: [S[_], A] => G[S, A] => H[S, A])(
          gKA: [S[_], A] => H[S, A] => G[S, A]
      ): CollectionOperation[H, F] = self.imapKA(fKA)(gKA)
