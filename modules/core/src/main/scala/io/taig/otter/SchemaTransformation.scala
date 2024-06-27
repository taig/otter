package io.taig.otter

type SchemaTransformation[F[+_], +G[a] <: Constraint.Any[a], A, B, C, D] = Transformation[
  A,
  G[(F[Schema[F, ?, B]], B)],
  (F[Schema[F, ?, C]], C),
  D
]

object SchemaTransformation:
  type Reader[F[+_], +G[a] <: Constraint.Any[a], A, B, C, D] = Transformation.Reader[
    A,
    G[(F[Schema[F, ?, B]], B)],
    (F[Schema[F, ?, C]], C),
    D
  ]

type CollectionTransformation[F[+_], A, B, C] = SchemaTransformation[F, [_] =>> Constraint.Collection, A, Nothing, B, C]

object CollectionTransformation:
  type Reader[F[+_], A, B, C] = SchemaTransformation.Reader[F, [_] =>> Constraint.Collection, A, Nothing, B, C]

type PrimitiveTransformation[F[+_], A, B, C, D] = SchemaTransformation[F, Constraint.Primitive, A, B, C, D]

object PrimitiveTransformation:
  type Reader[F[+_], A, B, C, D] = SchemaTransformation.Reader[F, Constraint.Primitive, A, B, C, D]
