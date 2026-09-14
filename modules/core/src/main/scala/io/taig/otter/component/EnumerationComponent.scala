package io.taig.otter.component

import cats.Order
import io.taig.enumeration.ext.EnumerationValues
import io.taig.enumeration.ext.Mapping
import io.taig.otter.Reference
import io.taig.otter.operation.EnumerationOperation

trait EnumerationComponent[Bound[-_, +_], F[_[-w, +r] <: Bound[w, r], -_, +_]]:
  def apply[S[-w, +r] <: Bound[w, r], A, B](schema: => S[A, A], mapping: Mapping[B, A])(using
      F: EnumerationOperation[F[S, *, *], S]
  ): F[S, B, B] = F.lift(Reference.later(schema), mapping)

  def apply[S[-w, +r] <: Bound[w, r], A: Order, B](schema: => S[A, A])(f: B => A)(using
      EnumerationValues.Aux[B, B],
      EnumerationOperation[F[S, *, *], S]
  ): F[S, B, B] = apply(schema, Mapping.enumeration(f))
