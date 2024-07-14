package io.taig.otter

import io.taig.otter as Base

trait Types:
  export Base.{Constraint, SchemaValidation, Type, ValidationWriter}

  object ValidationInvariant:
    type Collection[F[_]] = Base.ValidationInvariant[[_] =>> Constraint.Collection, ValidationWriter, F]

    type Primitive[F[_]] = Base.ValidationInvariant[
      [a] =>> Constraint.Primitive[ValidationWriter.Value[a]],
      ValidationWriter.Value,
      F
    ]
