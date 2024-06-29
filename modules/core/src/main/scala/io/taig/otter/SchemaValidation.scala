package io.taig.otter

import io.taig.otter.validation.Validation

type SchemaValidation[Contraint[+a] <: Constraint.Any[a], A, B, C, D] =
  Validation[A, Contraint[ValidationWriter[B]], ValidationWriter[C], D]

object SchemaValidation:
  type Collection[A, B, C] = SchemaValidation[[_] =>> Constraint.Collection, A, Nothing, B, C]

  type Object[A, B, C] = SchemaValidation[[_] =>> Constraint.Object, A, Nothing, B, C]

  type Primitive[A, B, C, D] = SchemaValidation[Constraint.Primitive, A, B, C, D]
