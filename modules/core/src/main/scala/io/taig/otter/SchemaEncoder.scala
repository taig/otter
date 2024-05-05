package io.taig.otter

trait SchemaEncoder[A]:
  def apply[B](schema: Schema[B], value: B): A
