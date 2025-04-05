package io.taig.otter

abstract class Printer[S[_]]:
  def apply[A](codec: S[A], a: A): String
