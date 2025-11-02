package io.taig.otter.component

import io.taig.otter.operation.UnionOperation

trait UnionComponent[+Self[_], -Value[_]](using operation: UnionOperation[Self, Value]):
  def branch[A](name: String, schema: => Value[A]): Self[A] = operation.apply(name, schema)
