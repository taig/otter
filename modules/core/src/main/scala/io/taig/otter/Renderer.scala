package io.taig.otter

abstract class Renderer[S[_], T]:
  def apply[A](codec: S[A]): T
