package io.taig.otter

import io.taig.otter.validation.Validation
import cats.Id as Identity

type SchemaValidation[F[+_], -A, B, C, +D] = Validation[
  A,
  (F[Schema.Writer[F, ?, B]], B),
  (F[Schema.Writer[F, ?, C]], C),
  D
]
