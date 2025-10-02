package io.taig.otter.codec

trait Encoder[-S[_], T]:
  def encode[A](schema: S[A], a: A): T