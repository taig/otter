package io.taig.otter.operation

import io.taig.otter.InvariantK
import cats.data.Chain
import io.taig.validation.Constraint
import io.taig.validation.Constraint.Primitive

trait PrimitiveOperation[Self[_]]:
  def constraints[A](self: Self[A]): Chain[Constraint.Primitive]

object PrimitiveOperation:
  inline def apply[Self[_]](using operation: PrimitiveOperation[Self]): PrimitiveOperation[Self] = operation

  given InvariantK[PrimitiveOperation] with
    extension [G[_]](operation: PrimitiveOperation[G])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): PrimitiveOperation[H] =
        new PrimitiveOperation[H]:
          override def constraints[A](self: H[A]): Chain[Primitive] = operation.constraints(gK(self))
