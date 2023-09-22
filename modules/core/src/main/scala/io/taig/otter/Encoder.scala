package io.taig.otter

trait Encoder[F[a] <: Schema[a], +A]:
  def encode[B](fb: F[B], b: B): A
