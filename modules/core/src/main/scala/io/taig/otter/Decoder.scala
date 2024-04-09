package io.taig.otter

import cats.data.Validated
import io.taig.otter.validation.Violations

abstract class Decoder[T]:
  final def decode[A](schema: Schema[?, A], value: T): Validated[Violations, A] = schema match
    case schema: Primitive[?, ?] => decode(schema, value)
    case schema: Tuple[?, ?]     => decode(schema, value)

  def decode[A](schema: Primitive[?, A], value: T): Validated[Violations, A]

  def decode[A](schema: Tuple[?, A], value: T): Validated[Violations, A]
