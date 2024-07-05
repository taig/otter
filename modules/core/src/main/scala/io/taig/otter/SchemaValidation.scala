package io.taig.otter

import io.taig.otter.validation.Validation

type SchemaValidation[Writer[+a] <: ValidationWriter[a], Contraint[+a] <: Constraint.Any[a], A, B, C, D] =
  Validation[A, Contraint[Writer[B]], Writer[C], D]

object SchemaValidation:
  type Collection[A, B, C] = SchemaValidation[ValidationWriter, [_] =>> Constraint.Collection, A, Nothing, B, C]

  type Object[A, B, C] = SchemaValidation[ValidationWriter, [_] =>> Constraint.Object, A, Nothing, B, C]

  type Primitive[A, B, C, D] = SchemaValidation[ValidationWriter.Value, Constraint.Primitive, A, B, C, D]
