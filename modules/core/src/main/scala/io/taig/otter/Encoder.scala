package io.taig.otter

abstract class Encoder[S[_], T]:
  def apply[A](codec: S[A], a: A): T
