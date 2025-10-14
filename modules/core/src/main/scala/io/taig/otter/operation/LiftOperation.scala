package io.taig.otter.operation

trait LiftOperation[+Self[_], -Value[_]]:
  def lift[A](value: => Value[A]): Self[A]
