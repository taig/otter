package io.taig.otter.component

import cats.Order
import io.taig.enumeration.ext.EnumerationValues
import io.taig.enumeration.ext.Mapping
import io.taig.otter.Reference
import io.taig.otter.operation.EnumerationOperation

trait EnumerationComponent[F[-_, +_], G[-_, +_]](using F: EnumerationOperation[F, G]):
  def apply[A, B](schema: => G[A, A], mapping: Mapping[B, A]): F[B, B] =
    F.lift(Reference.later(schema), mapping)

  def apply[A: Order, B](schema: => G[A, A])(f: B => A)(using EnumerationValues.Aux[B, B]): F[B, B] =
    apply(schema, Mapping.enumeration(f))
