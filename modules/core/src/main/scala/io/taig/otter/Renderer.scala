package io.taig.otter

abstract class Renderer[S[_], A]:
  def apply[T](codec: S[T]): A
