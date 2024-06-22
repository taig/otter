package io.taig.otter

import io.taig.otter.validation.Validation

type SchemaValidation[F[_], -A, B, C, +D] = Validation[A, (F[B], B), (F[C], C), D]
