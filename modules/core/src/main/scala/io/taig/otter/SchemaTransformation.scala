package io.taig.otter

type SchemaTransformation[Schema[_], Constraint[+a] <: Constraint.Any[a], A, B, C, D] =
  Transformation[A, Constraint[(Schema[B], B)], (Schema[C], C), D]

object SchemaTransformation:
  type Collection[A, B, C] = SchemaTransformation[Schema[Any, ?, *], [_] =>> Constraint.Collection, A, Nothing, B, C]

  type Object[A, B, C] = SchemaTransformation[Schema[Any, ?, *], [_] =>> Constraint.Object, A, Nothing, B, C]

  type Primitive[A, B, C, D] = SchemaTransformation[Value[Any, ?, *], Constraint.Primitive, A, B, C, D]
