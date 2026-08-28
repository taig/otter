package io.taig.otter.operation

import io.taig.otter.Reference

/** Constructs the coercion type `F` over schemas of type `G`. */
trait CoerceOperation[F[- _, + _], G[- _, + _]]:
  def lift[W, R](schema: Reference[G, W, R]): F[W, R]

  extension [W, R](fa: F[W, R]) def schema: Reference[G, ?, ?]

object CoerceOperation:
  inline def apply[F[- _, + _], G[- _, + _]](using self: CoerceOperation[F, G]): CoerceOperation[F, G] = self
