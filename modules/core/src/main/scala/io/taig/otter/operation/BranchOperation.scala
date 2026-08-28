package io.taig.otter.operation

import io.taig.otter.Reference

/** Constructs the branch type `F` of a union whose schemas have type `G`. */
trait BranchOperation[F[- _, + _], G[- _, + _]]:
  def lift[W, R](name: String, schema: Reference[G, W, R]): F[W, R]

  extension [W, R](fa: F[W, R])
    def name: String
    def schema: Reference[G, ?, ?]

object BranchOperation:
  inline def apply[F[- _, + _], G[- _, + _]](using self: BranchOperation[F, G]): BranchOperation[F, G] = self
