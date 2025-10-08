package io.taig.otter.operation

import io.taig.otter.FunctorK

trait LiftOperation[-Shape[_], +Self[_[a] <: Shape[a], _]]:
  def lift[Value[a] <: Shape[a], A](value: => Value[A]): Self[Value, A]
