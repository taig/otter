package io.taig.otter

type SchemaTransformation[F[+_], A, +B <: Constraint.Any[?], C, D] = Transformation[A, B, (F[Schema[F, ?, C]], C), D]

object SchemaTransformation:
  type Reader[F[+_], A, +B <: Constraint.Any[?], C, D] = Transformation.Reader[A, B, (F[Schema[F, ?, C]], C), D]

type CollectionTransformation[F[+_], A, B, C] = SchemaTransformation[F, A, Constraint.Collection, B, C]

object CollectionTransformation:
  type Reader[F[+_], A, B, C] = SchemaTransformation.Reader[F, A, Constraint.Collection, B, C]
