package io.taig.otter.operation

trait FieldOperation[Self[_], -Value[_]]:
  def apply[A](name: String, value: => Value[A]): Self[A]

  def optional[A](self: Self[A]): Self[Option[A]]

  def optional[A](self: Self[A], default: => A): Self[A]

object FieldOperation:
  inline def apply[Self[_], Value[_]](using
      operation: FieldOperation[Self, Value]
  ): FieldOperation[Self, Value] = operation
