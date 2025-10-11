package io.taig.otter.operation

import io.taig.otter.FunctorK
import scala.deriving.Mirror

trait BooleanOperation[+Self[_]]:
  self =>

  def boolean: Self[Boolean]

object BooleanOperation:
  inline def apply[Self[_]](using operation: BooleanOperation[Self]): BooleanOperation[Self] = operation

  given FunctorK[BooleanOperation] with
    extension [G[_]](self: BooleanOperation[G])
      override def mapK[H[_]](fK: [A] => G[A] => H[A]): BooleanOperation[H] =
        new BooleanOperation[H]:
          override def boolean: H[Boolean] = fK(self.boolean)
