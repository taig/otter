package io.taig.otter

type SchemaTransformation[Writer[_], Constraint[+a] <: Constraint.Any[a], A, B, C, D] =
  Transformation[A, Constraint[(Writer[B], B)], (Writer[C], C), D]

object SchemaTransformation:
  type Collection[Writer[_], A, B, C] =
    SchemaTransformation[Writer, [_] =>> Constraint.Collection, A, Nothing, B, C]

  type Object[Writer[_], A, B, C] = SchemaTransformation[Writer, [_] =>> Constraint.Object, A, Nothing, B, C]

  type Primitive[Writer[_], A, B, C, D] = SchemaTransformation[Writer, Constraint.Primitive, A, B, C, D]
