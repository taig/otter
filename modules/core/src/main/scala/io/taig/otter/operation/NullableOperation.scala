package io.taig.otter.operation

trait NullableOperation[+Self[_], -Value[_]]:
  def nullable[A](value: => Value[A]): Self[Option[A]]

  def nullable[A](value: => Value[A], default: => A): Self[A]

object NullableOperation:
  inline def apply[Self[_], Value[_]](using
      operation: NullableOperation[Self, Value]
  ): NullableOperation[Self, Value] = operation
