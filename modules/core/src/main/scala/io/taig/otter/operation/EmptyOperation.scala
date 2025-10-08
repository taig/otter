package io.taig.otter.operation

trait EmptyOperation[+Self[_[_] <: Nothing, _]]:
  def empty: Self[Nothing, Unit]
