package io.taig.otter

import cats.data.Validated
import io.taig.otter.validation.Violations

abstract class Decoder[T]:
  final def decode[A](schema: Schema[?, A], value: T): Validated[Violations[T], A] = schema match
    case schema: Collection[?, ?] => decode(schema, value)
    // case schema: Primitive[?, ?]  => decode(schema, value)
    // case schema: Tuple[?, ?]      => decode(schema, value)
    // case schema: Union[?, ?]      => decode(schema, value)

  def decode[A](schema: Collection[?, A], value: T): Validated[Violations[T], A]
  // def decode[A](schema: Primitive[?, A], value: T): Validated[Violations[T], A]
  // def decode[A](schema: Tuple[Schema[?], A], value: T): Validated[Violations[T], A]
  // def decode[A](schema: Union[Schema[?], A], value: T): Validated[Violations[T], A]
