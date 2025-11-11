package io.taig.otter.instances

import cats.Invariant
import io.taig.otter.WrapperK2

trait CatsInstances:
  implicit def invariantWrapperK2[G[+_[a] <: I[a], _], H[+_[a] <: I[a], _], I[_]](using
      W: WrapperK2[G, H, I],
      F: Invariant[H[I, *]]
  ): Invariant[G[I, *]] = ???

object CatsInstances extends CatsInstances
