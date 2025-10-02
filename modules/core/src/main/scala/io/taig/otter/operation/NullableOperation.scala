package io.taig.otter.operation

import io.taig.otter.FunctorK

trait NullableOperation[+Self[_], -Value[_]]:
  self =>

  def nullable[A](value: => Value[A]): Self[Option[A]]

  def nullable[A](value: => Value[A], default: => A): Self[A]

  def mapK[T[_]](fK: [A] => Self[A] => T[A]): NullableOperation[T, Value] =
    new NullableOperation[T, Value]:
      def nullable[A](value: => Value[A]): T[Option[A]] = fK(self.nullable(value))
      def nullable[A](value: => Value[A], default: => A): T[A] = fK(self.nullable(value, default))

object NullableOperation:
  inline def apply[Self[_], Value[_]](using
      operation: NullableOperation[Self, Value]
  ): NullableOperation[Self, Value] = operation

  given [Value[_]]: FunctorK[[s[_]] =>> NullableOperation[s, Value]] with
    extension [G[_]](self: NullableOperation[G, Value])
      override def mapK[H[_]](fK: [A] => G[A] => H[A]): NullableOperation[H, Value] =
        self.mapK(fK)
