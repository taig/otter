package io.taig.otter.operation

import io.taig.otter.FunctorK

trait NullableOperation[+Self[_], -Value[_]]:
  def nullable[A](value: => Value[A]): Self[Option[A]]

  def nullable[A](value: => Value[A], default: => A): Self[A]

object NullableOperation:
  inline def apply[Self[_], Value[_]](using
      operation: NullableOperation[Self, Value]
  ): NullableOperation[Self, Value] = operation

  given [Value[_]]: FunctorK[[f[_]] =>> NullableOperation[f, Value]] with
    extension [G[_]](self: NullableOperation[G, Value])
      override def mapK[H[_]](fK: [A] => G[A] => H[A]): NullableOperation[H, Value] =
        new NullableOperation[H, Value]:
          def nullable[A](value: => Value[A]): H[Option[A]] = fK(self.nullable(value))
          def nullable[A](value: => Value[A], default: => A): H[A] = fK(self.nullable(value, default))
