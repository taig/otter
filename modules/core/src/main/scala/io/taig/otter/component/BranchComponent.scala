package io.taig.otter.component

import io.taig.otter.operation.BranchOperation

trait BranchComponent[+Self[_], -Value[_]](using operation: BranchOperation[Self, Value]):
  def branch[A](name: String, value: => Value[A]): Self[A] = operation.apply(name, value)
