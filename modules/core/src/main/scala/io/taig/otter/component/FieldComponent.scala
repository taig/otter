package io.taig.otter.component

import io.taig.otter.operation.FieldOperation

trait FieldComponent[+Self[_], -Value[_]](using operation: FieldOperation[Self, Value]):
  def field[A](name: String, value: => Value[A]): Self[A] = operation.apply(name, value)
