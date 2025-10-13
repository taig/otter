package io.taig.otter.operation

trait EmptyOperation[+Self[_]]:
  def empty: Self[Unit]
