package io.taig.otter

import cats.data.Validated
import io.taig.otter.validation.Violations

// abstract class Decoder[T]:
//   final def apply[A](schema: Schema[A], value: T): Validated[Violations, A] = schema match
//     case schema: Primitive[?] => apply(schema, value)
//     case schema: Tuple[?]     => apply(schema, value)

//   def apply[A](schema: Primitive[A], value: T): Validated[Violations, A]

//   def apply[A](schema: Tuple[A], value: T): Validated[Violations, A]
