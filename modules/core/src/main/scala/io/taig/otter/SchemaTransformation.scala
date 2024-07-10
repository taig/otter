package io.taig.otter

type SchemaTransformation[Constraint[+a] <: Constraint.Any[a], A, B, C, D] =
  Transformation[A, Constraint[ValidationWriter[B]], ValidationWriter[C], D]

object SchemaTransformation:
  type Collection[A, B, C] = SchemaTransformation[[_] =>> Constraint.Collection, A, Nothing, B, C]

  type Object[A, B, C] = SchemaTransformation[[_] =>> Constraint.Object, A, Nothing, B, C]

  type Primitive[A, B, C, D] = SchemaTransformation[Constraint.Primitive, A, B, C, D]
