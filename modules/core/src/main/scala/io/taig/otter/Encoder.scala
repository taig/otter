package io.taig.otter

trait Encoder[S[_], A]:
  def apply[B](schema: S[B], b: B): A
