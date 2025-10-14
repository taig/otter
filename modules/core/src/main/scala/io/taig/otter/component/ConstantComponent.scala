package io.taig.otter.component

import cats.Eq
import io.taig.otter.operation.ConstantOperation

trait ConstantComponent[+Self[_], -Value[_]](using operation: ConstantOperation[Self, Value]):
  def constant[A: Eq](schema: => Value[A], value: A): Self[A] = operation.constant(schema, value)
