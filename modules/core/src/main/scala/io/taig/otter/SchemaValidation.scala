package io.taig.otter

import io.taig.otter.validation.Validation

type SchemaValidation[F[+_], G[a] <: Constraint.Any[a], A, B, C, D] = Validation[
  A,
  G[(F[Schema.Writer[F, ?, C]], B)],
  (F[Schema.Writer[F, ?, C]], C),
  D
]

type CollectionValidation[F[+_], A, B, C] = SchemaValidation[F, [_] =>> Constraint.Collection, A, Nothing, B, C]

type PrimitiveValidation[F[+_], A, B, C, D] = SchemaValidation[F, Constraint.Primitive, A, B, C, D]
