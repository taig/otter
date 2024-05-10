package io.taig.otter

import io.taig.otter.validation.Validation

type SchemaValidation[-In, A, B, +Out] = Validation[
  In,
  (Fix[Schema.Writer[*, A]], A),
  (Fix[Schema.Writer[*, B]], B),
  Out
]
