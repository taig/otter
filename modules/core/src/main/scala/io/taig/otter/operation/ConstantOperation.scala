package io.taig.otter.operation

import cats.Eq
import io.taig.otter.FunctorK

trait ConstantOperation[+Self[_], -Value[_]]:
  def constant[A: Eq](schema: => Value[A], value: A): Self[A]

object ConstantOperation:
  inline def apply[Self[_], Value[_]](using
      operation: ConstantOperation[Self, Value]
  ): ConstantOperation[Self, Value] = operation

  given [Value[_]]: FunctorK[[f[_]] =>> ConstantOperation[f, Value]] with
    extension [G[_]](self: ConstantOperation[G, Value])
      override def mapK[H[_]](fK: [A] => G[A] => H[A]): ConstantOperation[H, Value] =
        new ConstantOperation[H, Value]:
          override def constant[A: Eq](schema: => Value[A], value: A): H[A] = fK(self.constant(schema, value))
