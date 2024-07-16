package io.taig.otter

import io.taig.otter as Base

trait Types:
  export Base.{CodecValidation, Constraint, Type}

  final type Schema[A] = Base.Codec[?, A]

  object Schema:
    type Of[A, B] = Base.Codec[A, B]

  // final type Value[A] = Base.Schema[?, A]

  // object Value:
  //   type Of[A, B] = Base.Value[A, B]

  // final type Enumeration[A] = Base.Enumeration[?, A]

  // object Enumeration:
  //   type Of[A, B] = Base.Enumeration[A, B]

  //   type Required[A] = Base.Enumeration.Required[?, A]

  //   object Required:
  //     type Of[A, B] = Base.Enumeration.Required[A, B]

object Types extends Types
