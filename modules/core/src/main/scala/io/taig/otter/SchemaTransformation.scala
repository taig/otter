package io.taig.otter

type SchemaTransformation[Constraint[+a] <: Constraint.Any[a], A, B] =
  Transformation[A, Constraint[Data], Data, B]

object SchemaTransformation:
  type Collection[A, B] = SchemaTransformation[[_] =>> Constraint.Collection, A, B]

  type Object[A, B] = SchemaTransformation[[_] =>> Constraint.Object, A, B]

  type Primitive[A, B] = SchemaTransformation[Constraint.Primitive, A, B]
