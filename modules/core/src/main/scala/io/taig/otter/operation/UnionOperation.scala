package io.taig.otter.operation

import io.taig.otter.FunctorK

trait UnionOperation[Self[_], -Value[_]] extends LiftOperation[Self, Value]:
  self =>

  def mapK[T[_]](fK: [A] => Self[A] => T[A]): UnionOperation[T, Value] = new UnionOperation[T, Value] {
    override def lift[A](value: => Value[A]): T[A] = fK(self.lift(value))
  }

object UnionOperation:
  inline def apply[Self[_], Value[_]](using operation: UnionOperation[Self, Value]): UnionOperation[Self, Value] =
    operation

  given [Value[_]]: FunctorK[[s[_]] =>> UnionOperation[s, Value]] with
    extension [G[_]](self: UnionOperation[G, Value])
      override def mapK[H[_]](fK: [A] => G[A] => H[A]): UnionOperation[H, Value] = self.mapK(fK)
