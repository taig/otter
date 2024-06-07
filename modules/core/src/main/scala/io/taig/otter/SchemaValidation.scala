package io.taig.otter

import io.taig.otter.validation.Validation
import cats.Id as Identity

type SchemaValidation[-A, B, C, +D] = Validation[
  A,
  (Schema.Writer[Any, B], B),
  (Schema.Writer[Any, C], C),
  D
]
