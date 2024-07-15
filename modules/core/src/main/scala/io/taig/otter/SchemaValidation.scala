package io.taig.otter

import io.taig.otter.validation.Validation

type SchemaValidation[Contraint[+a] <: Constraint.Any[a], A, B] =
  Validation[A, Contraint[Data], Data, B]

object SchemaValidation:
  type Collection[A, B] = SchemaValidation[[_] =>> Constraint.Collection, A, B]

  type Object[A, B] = SchemaValidation[[_] =>> Constraint.Object, A, B]

  type Primitive[A, B] = SchemaValidation[Constraint.Primitive, A, B]
