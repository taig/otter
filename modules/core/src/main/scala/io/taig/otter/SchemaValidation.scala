package io.taig.otter

import io.taig.otter.validation.Validation

type SchemaValidation[-In, A, B, +Out] = Validation[
  In,
  (Schema.Writer.Any[Schema.Writer.Identity[A], A], A),
  (Schema.Writer.Any[Schema.Writer.Identity[B], B], B),
  Out
]
