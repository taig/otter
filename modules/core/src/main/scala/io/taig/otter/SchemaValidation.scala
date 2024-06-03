package io.taig.otter

import io.taig.otter.validation.Validation
import cats.Id as Identity

type SchemaValidation[-A, B, C, +D] = Validation[
  A,
  (Schema.Writer[Identity, ?, B], B),
  (Schema.Writer[Identity, ?, C], C),
  D
]
