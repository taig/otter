package io.taig.otter.operation

import io.taig.otter.FunctorK

trait LiftOperation[+Self[_], -Value[_]]:
  def lift[A](value: => Value[A]): Self[A]

object LiftOperation:
  given [Value[_]]: FunctorK[[s[_]] =>> LiftOperation[s, Value]] with
    extension [G[_]](self: LiftOperation[G, Value])
      override def mapK[H[_]](fK: [A] => G[A] => H[A]): LiftOperation[H, Value] =
        new LiftOperation[H, Value]:
          override def lift[A](value: => Value[A]): H[A] = fK(self.lift(value))
