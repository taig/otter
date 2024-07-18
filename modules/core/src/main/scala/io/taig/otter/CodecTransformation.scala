package io.taig.otter

type CodecTransformation[Constraint[+a] <: Constraint.Any[a], A, B] =
  Transformation[A, Constraint[Data], Data, B]

object CodecTransformation:
  type Collection[A, B] = CodecTransformation[[_] =>> Constraint.Collection, A, B]

  type Object[A, B] = CodecTransformation[[_] =>> Constraint.Object, A, B]

  type Primitive[A, B] = CodecTransformation[Constraint.Primitive, A, B]
