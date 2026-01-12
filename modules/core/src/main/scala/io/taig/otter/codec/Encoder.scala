package io.taig.otter.codec

trait Encoder[F[_], A]:
  def encode[B](fb: F[B], b: B): A
