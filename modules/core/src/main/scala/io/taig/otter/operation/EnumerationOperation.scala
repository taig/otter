package io.taig.otter.operation

import io.taig.enumeration.ext.Mapping
import io.taig.otter.Reference

/** Constructs the enumeration type `F` over representation schemas of type `G`. */
trait EnumerationOperation[F[-_, +_], G[-_, +_]]:
  def lift[A, B](schema: Reference[G, A, A], mapping: Mapping[B, A]): F[B, B]

  extension [W, R](fa: F[W, R]) def schema: Reference[G, ?, ?]

object EnumerationOperation:
  inline def apply[F[-_, +_], G[-_, +_]](using self: EnumerationOperation[F, G]): EnumerationOperation[F, G] =
    self
