package io.taig.otter.component

import io.taig.otter.operation.FieldOperation

trait FieldComponent[-Shape[_], Self[_[a] <: Shape[a], _]]:
  def field[Value[a] <: Shape[a], A](name: String, value: => Value[A])(using
      operation: FieldOperation[Self[Value, *], Value]
  ): Self[Value, A] = operation.apply(name, value)
