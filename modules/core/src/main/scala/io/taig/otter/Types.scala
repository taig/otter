package io.taig.otter

import io.taig.otter as Base

trait Types[F[+_]]:
  final type Schema[+A, B] = F[Base.Schema[A, B]]

  object Schema:
    type Reader[+A, +B] = F[Base.Schema.Reader[A, B]]
    type Writer[+A, -B] = F[Base.Schema.Writer[A, B]]

  final type Primitive[A] = F[Base.Primitive[A]]

  object Primitive:
    // type Required[A] = F[[_] =>> Base.Primitive.Required[A]]

    // object Required:
    //   type Reader[+A] = F[[_] =>> Base.Primitive.Required.Reader[A]]
    //   type Writer[-A] = F[[_] =>> Base.Primitive.Required.Writer[A]]

    type Reader[+A] = F[Base.Primitive.Reader[A]]
    type Writer[-A] = F[Base.Primitive.Writer[A]]

  final type Tuple[+A, B] = F[Base.Tuple[A, B]]

  object Tuple:
    type Reader[+A, +B] = F[Base.Tuple.Reader[A, B]]
    type Writer[+A, -B] = F[Base.Tuple.Writer[A, B]]
