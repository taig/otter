package io.taig.otter

trait Encoder[F[a] <: Schema[a], +A]:
  def encode[B](schema: F[B], b: B): A
