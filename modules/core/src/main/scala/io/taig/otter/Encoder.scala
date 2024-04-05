package io.taig.otter

abstract class Encoder[T]:
  final def apply[A](schema: Schema[A], value: A): T = schema match
    case schema: Primitive[?] => apply(schema, value)
    case schema: Product[?]   => apply(schema, value)

  def apply[A](schema: Primitive[A], value: A): T

  def apply[A](schema: Product[A], value: A): T
