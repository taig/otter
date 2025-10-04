package io.taig.otter.operation

import io.taig.otter.FunctorK

trait EmptyOperation[+Self[_]]:
  self =>

  def empty: Self[Unit]

object EmptyOperation:
  given FunctorK[EmptyOperation] with
    extension [G[_]](self: EmptyOperation[G])
      override def mapK[H[_]](fK: [A] => G[A] => H[A]): EmptyOperation[H] =
        new EmptyOperation[H]:
          override def empty: H[Unit] = fK(self.empty)
