package io.taig.otter

import io.taig.otter as Base

trait Types[S[+_]]:
  type Schema[A] = Schema.Of[S[Base.Schema.Any[S, ?]], A]

  object Schema:
    type Of[+A <: S[Base.Schema.Any[S, ?]], B] = S[Base.Schema[Base.Data[S, A, *], B]]

  type Primitive[A] = S[Base.Schema[Base.Primitive, A]]

  type Tuple[A] = Tuple.Of[S[Base.Schema.Any[S, ?]], A]

  object Tuple:
    type Of[+A <: S[Base.Schema.Any[S, ?]], B] = S[Base.Schema[Base.Tuple[S, A, *], B]]
