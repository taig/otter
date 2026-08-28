package io.taig.otter.operation

import io.taig.otter.Reference

/** What `:+` needs in order to add `H` to `F`: a way to lift the receiver into the union `G` that accumulates, and a
  * way to lift a branch into it.
  *
  * Mirrors [[AppendableOperation]], and for the same reason: naming the receiver, the result and the element in one
  * instance puts them all into a single implicit search. Split across `UnionableOperation[F, G]` and
  * `UnionOperation[G, H]`, the first names only `F`, so `G` is still a free type variable when it is solved and the
  * second has nothing to match.
  */
trait AlternableOperation[F[-_, +_], G[-_, +_], -H[-_, +_]]:
  def lift[W, R](fa: F[W, R]): G[W, R]

  def element[W, R](fb: => H[W, R]): G[W, R]

object AlternableOperation:
  inline def apply[F[-_, +_], G[-_, +_], H[-_, +_]](using
      self: AlternableOperation[F, G, H]
  ): AlternableOperation[F, G, H] = self

  /** Deliberately not a `given`, for the reason spelled out on [[AppendableOperation]]: a format registers the one
    * instance that fits each receiver in that receiver's companion.
    */
  def union[F[-_, +_], G[-_, +_], H[-_, +_]](using
      U: UnionableOperation[F, G],
      O: UnionOperation[G, H]
  ): AlternableOperation[F, G, H] = new AlternableOperation[F, G, H]:
    override def lift[W, R](fa: F[W, R]): G[W, R] = U.toUnion(fa)
    override def element[W, R](fb: => H[W, R]): G[W, R] = O.lift(Reference.later(fb))
