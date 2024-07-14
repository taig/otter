package io.taig.otter

import io.taig.otter.validation.Validation

type SchemaValidation[Schema[_], Contraint[+a] <: Constraint.Any[a], A, B, C, D] =
  Validation[A, Contraint[(Schema[B], B)], (Schema[C], C), D]

object SchemaValidation:
  type Collection[A, B, C] = SchemaValidation[Schema[?, *], [_] =>> Constraint.Collection, A, Nothing, B, C]

  type Object[A, B, C] = SchemaValidation[Schema[?, *], [_] =>> Constraint.Object, A, Nothing, B, C]

  type Primitive[A, B, C, D] = SchemaValidation[Value[?, *], Constraint.Primitive, A, B, C, D]
