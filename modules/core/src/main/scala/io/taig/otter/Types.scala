package io.taig.otter

import io.taig.otter as Base

trait Types[S[+_]]:
  type Schema[A] = Schema.Of[Base.Schema[S, ?, ?], A]

  object Schema:
    type Of[+A, B] = S[Base.Schema[S, A, B]]

    type Reader[+A] = Schema.Reader.Of[Base.Schema.Reader[S, ?, ?], A]

    object Reader:
      type Of[+A, +B] = S[Base.Schema.Reader[S, A, B]]

  type Primitive[A] = S[Base.Primitive[A]]

  object Primitive:
    type Required[A] = S[Base.Primitive.Required[A]]

    type Reader[+A] = S[Base.Primitive.Reader[A]]

    type Writer[-A] = S[Base.Primitive.Writer[A]]

  type Tuple[A] = Tuple.Of[Base.Schema[S, ?, ?], A]

  object Tuple:
    type Of[+A, B] = S[Base.Tuple[S, A, B]]

    type Reader[+A] = Tuple.Reader.Of[Base.Schema.Reader[S, ?, ?], A]

    object Reader:
      type Of[+A, +B] = S[Base.Tuple.Reader[S, A, B]]
