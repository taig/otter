package io.taig.otter.operation

import io.taig.otter.FunctorK

trait LiftOperation[+Self[_], -Value[_]]:
  def lift[A](value: => Value[A]): Self[A]
