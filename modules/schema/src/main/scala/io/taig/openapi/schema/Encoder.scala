package io.taig.openapi.schema

abstract class Encoder[F[_], +A]:
  def encode[B](fb: F[B], b: B): A
