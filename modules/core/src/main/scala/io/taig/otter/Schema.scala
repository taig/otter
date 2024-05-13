package io.taig.otter

type Schema[+S[+_], +A, B] = Primitive[B] | Tuple[S, A, B]

object Schema:
  type Reader[+S[+_], +A, +B] = Primitive.Reader[B] | Tuple.Reader[S, A, B]

  type Writer[+S[+_], +A, -B] = Primitive.Writer[B] | Tuple.Writer[S, A, B]
