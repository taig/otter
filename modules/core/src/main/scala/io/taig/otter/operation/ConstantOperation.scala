package io.taig.otter.operation

import cats.Eq
import io.taig.otter.FunctorK

trait ConstantOperation[+Self[_], -Value[_]]:
  self =>

  def constant[A: Eq](schema: => Value[A], value: A): Self[A]

  def mapK[T[_]](fK: [A] => Self[A] => T[A]): ConstantOperation[T, Value] = new ConstantOperation[T, Value]:
    override def constant[A: Eq](schema: => Value[A], value: A): T[A] = fK(self.constant(schema, value))

object ConstantOperation:
  inline def apply[Self[_], Value[_]](using operation: ConstantOperation[Self, Value]): ConstantOperation[Self, Value] =
    operation

  given [Value[_]]: FunctorK[[s[_]] =>> ConstantOperation[s, Value]] with
    extension [G[_]](self: ConstantOperation[G, Value])
      override def mapK[H[_]](fK: [A] => G[A] => H[A]): ConstantOperation[H, Value] =
        self.mapK(fK)
