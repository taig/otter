package io.taig.otter

import io.taig.otter.validation.Validation

// type SchemaValidation[-In, A, B, +Out] = Validation[In, (Plain.Schema.Writer[A], A), (Plain.Schema.Writer[B], B), Out]
type SchemaValidation[-In, A, B, +Out] = Validation[In, (A), (B), Out]
