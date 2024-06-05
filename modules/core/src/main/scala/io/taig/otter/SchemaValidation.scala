package io.taig.otter

import io.taig.otter.validation.Validation
import cats.Id as Identity

type SchemaValidation[-A, B, C, +D] = Validation[
  A,
  (Schema.Writer[Any, Any, B], B),
  (Schema.Writer[Any, Any, C], C),
  D
]
