package io.taig.otter

import io.taig.otter.validation.Validation

type CodecValidation[C[+a] <: Constraint.Any[a], A, B] = Validation[A, C[Data] | Constraint[Data], Data, B]

object CodecValidation:
  type Collection[A, B] = CodecValidation[[_] =>> Constraint.Collection, A, B]

  type Object[A, B] = CodecValidation[[_] =>> Constraint.Object, A, B]

  type Primitive[A, B] = CodecValidation[Constraint.Primitive, A, B]
