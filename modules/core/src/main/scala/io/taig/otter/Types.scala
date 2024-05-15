package io.taig.otter

import io.taig.otter as Base

trait Types:
  // type Schema[A] = S[Base.Schema[Codec[Any, *], A]]

  // type Codec[+A, B] = Base.Collection[S, A, B] | Base.Primitive[B] | Base.Tuple[S, A, B]

  type ParentX = Fix[[a] =>> Base.Schema[Base.Codec[a, *], ?]]
  type ParentA[A] = Fix[[a] =>> Base.Schema[Base.Codec[a, *], A]]

  type Tuple[A] = Base.Schema[Base.Tuple[ParentA[A], *], A]
