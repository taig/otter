package io.taig.otter

import io.taig.otter.validation.Validation

type SchemaValidation[M, -A, B, C, +D] = Validation[A, (Schema[M, M, ?, B], B), (Schema[M, M, ?, C], C), D]
