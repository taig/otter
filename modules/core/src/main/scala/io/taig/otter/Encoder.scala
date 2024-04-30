package io.taig.otter

abstract class Encoder[T]:
  final def encode[A](schema: Schema[?, A], a: A): T = schema match
    case schema: Collection[?, ?] => encode(schema, a)
    // case schema: Primitive[?, ?]  => encode(schema, a)
    // case schema: Tuple[?, ?]      => encode(schema, a)
    // case schema: Union[?, ?]      => encode(schema, a)

  def encode[A](schema: Collection[Schema[?, ?], A], a: A): T
  // def encode[A](schema: Primitive[?, A], a: A): T
  // def encode[A](schema: Tuple[Schema[?], A], a: A): T
  // def encode[A](schema: Union[Schema[?], A], a: A): T
