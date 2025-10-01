package io.taig.otter.operation

import io.taig.otter.FunctorK

trait BooleanOperation[+Self[_]]:
  self =>

  def boolean: Self[Boolean]

  def mapK[G[_]](fK: [A] => Self[A] => G[A]): BooleanOperation[G] = new BooleanOperation[G]:
    override def boolean: G[Boolean] = fK(self.boolean)

object BooleanOperation:
  inline def apply[Self[_]](using operation: BooleanOperation[Self]): BooleanOperation[Self] = operation

  given FunctorK[BooleanOperation] with
    extension [G[_]](self: BooleanOperation[G])
      override def mapK[H[_]](fK: [A] => G[A] => H[A]): BooleanOperation[H] = self.mapK(fK)
