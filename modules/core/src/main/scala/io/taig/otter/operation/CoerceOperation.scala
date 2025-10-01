package io.taig.otter.operation

import io.taig.otter.FunctorK

trait CoerceOperation[+Self[_], -Value[_]]:
  self =>

  def coerce[A](schema: => Value[A]): Self[A]

  def mapK[T[_]](fK: [A] => Self[A] => T[A]): CoerceOperation[T, Value] =
    new CoerceOperation[T, Value]:
      override def coerce[A](schema: => Value[A]): T[A] = fK(self.coerce(schema))

object CoerceOperation:
  inline def apply[Self[_], Value[_]](using operation: CoerceOperation[Self, Value]): CoerceOperation[Self, Value] =
    operation

  given [Value[_]]: FunctorK[[s[_]] =>> CoerceOperation[s, Value]] with
    extension [G[_]](self: CoerceOperation[G, Value])
      override def mapK[H[_]](fK: [A] => G[A] => H[A]): CoerceOperation[H, Value] = self.mapK(fK)
