package io.taig.otter

import io.taig.otter.validation.Validation

type SchemaValidation[-In, A, B, +Out] = Validation[
  In,
  (Schema.Writer[Schema.Writer.Identity[A], A], A),
  (Schema.Writer[Schema.Writer.Identity[B], B], B),
  Out
]
