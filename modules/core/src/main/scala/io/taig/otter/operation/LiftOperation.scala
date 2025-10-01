package io.taig.otter.operation

import io.taig.otter.FunctorK

trait LiftOperation[+Self[_], -Value[_]]:
  self =>

  def lift[A](value: => Value[A]): Self[A]

  def mapK[G[_]](fK: [A] => Self[A] => G[A]): LiftOperation[G, Value] = new LiftOperation[G, Value]:
    def lift[A](value: => Value[A]): G[A] = fK(self.lift(value))

object LiftOperation:
  given [Value[_]]: FunctorK[[F[_]] =>> LiftOperation[F, Value]] with
    extension [G[_]](self: LiftOperation[G, Value])
      override def mapK[H[_]](fK: [A] => G[A] => H[A]): LiftOperation[H, Value] =
        self.mapK(fK)
