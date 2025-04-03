package io.taig.otter

abstract class Renderer[A, B]:
  def apply(value: A): B
