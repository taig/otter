package io.taig.crock.schema

trait Encoder[F[_], +A]:
  def encode[B](fb: F[B], b: B): A
