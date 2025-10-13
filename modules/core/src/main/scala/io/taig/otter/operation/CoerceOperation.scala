package io.taig.otter.operation

import io.taig.otter.FunctorK

trait CoerceOperation[+Self[_], -Value[_]]:
  def coerce[A](schema: => Value[A]): Self[A]

object CoerceOperation:
  inline def apply[Self[_], Value[_]](using
      operation: CoerceOperation[Self, Value]
  ): CoerceOperation[Self, Value] = operation

  given [Value[_]]: FunctorK[[s[_]] =>> CoerceOperation[s, Value]] with
    extension [G[_]](self: CoerceOperation[G, Value])
      override def mapK[H[_]](fK: [A] => G[A] => H[A]): CoerceOperation[H, Value] =
        new CoerceOperation[H, Value]:
          override def coerce[A](schema: => Value[A]): H[A] = fK(self.coerce(schema))
