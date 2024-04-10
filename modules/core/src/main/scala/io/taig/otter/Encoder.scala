package io.taig.otter

abstract class Encoder[T]:
  final def encode[A](schema: Schema[A], value: A): T = schema match
    case schema: Primitive[?] => encode(schema, value)
    case schema: Tuple[?, ?]  => encode(schema, value)
    case schema: Union[?, ?]  => encode(schema, value)

  def encode[A](schema: Primitive[A], value: A): T

  def encode[A](schema: Tuple[Schema[?], A], value: A): T

  def encode[A](schema: Union[Schema[?], A], value: A): T
