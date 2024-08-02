package io.taig.otter

import io.taig.otter.validation.Validation

type CodecValidation[+O, A, B] = Validation[A, Constraint[O], Data, B]

object CodecValidation:
  type Any[A, B] = CodecValidation[Nothing, A, B]

  type Primitive[A, B] = CodecValidation[Data.Primitive, A, B]

  type Array[A, B] = CodecValidation[Data.Array[?], A, B]

  type Object[A, B] = CodecValidation[Data.Object[?], A, B]
