package io.taig.otter.component

import cats.Order
import io.taig.enumeration.ext.EnumerationValues
import io.taig.enumeration.ext.Mapping
import io.taig.otter.Reference
import io.taig.otter.operation.EnumerationOperation

trait EnumerationComponent[F[-_, +_], F1[A] >: F[A, A] <: F[A, A], G[-_, +_], G1[A] >: G[A, A] <: G[A, A]](using
    F: EnumerationOperation[F, G]
):
  def apply[A, B](schema: => G1[A], mapping: Mapping[B, A]): F1[B] =
    F.lift(Reference.later(schema), mapping)

  def apply[A: Order, B](schema: => G1[A])(f: B => A)(using EnumerationValues.Aux[B, B]): F1[B] =
    apply(schema, Mapping.enumeration(f))
