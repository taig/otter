package io.taig.otter.component

import io.taig.otter.operation.ConstantOperation
import cats.Eq

trait ConstantComponent[-Shape[_], Self[_[a] <: Shape[a], _]]:
  def constant[Value[a] <: Shape[a], A: Eq](schema: => Value[A], value: A)(using
      operation: ConstantOperation[Self[Value, *], Value]
  ): Self[Value, A] = operation.constant(schema, value)
