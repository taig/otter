package io.taig.otter

import io.taig.otter.validation.Validation

type SchemaValidation[Writer[_], Contraint[+a] <: Constraint.Any[a], A, B, C, D] =
  Validation[A, Contraint[(Writer[B], B)], (Writer[C], C), D]

object SchemaValidation:
  type Collection[Writer[_], A, B, C] =
    SchemaValidation[Writer, [_] =>> Constraint.Collection, A, Nothing, B, C]

  type Object[Writer[_], A, B, C] = SchemaValidation[Writer, [_] =>> Constraint.Object, A, Nothing, B, C]

  type Primitive[Writer[_], A, B, C, D] = SchemaValidation[Writer, Constraint.Primitive, A, B, C, D]
