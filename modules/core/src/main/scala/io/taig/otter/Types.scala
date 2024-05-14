package io.taig.otter

import io.taig.otter as Base

trait Types[S[+_]]
// type All[A] = Fix[[a] =>> Base.Collection[a, A] | Base.Primitive[A] | Base.Tuple[a, A]]

// type Schema[A] = S[Base.Schema[?, A]]

// type Tuple[+A, B] = S[Base.Schema[Base.Tuple[A, *], B]]
