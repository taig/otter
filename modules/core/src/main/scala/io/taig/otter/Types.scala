package io.taig.otter

import io.taig.otter as Base

trait Types:
  type AsSchema[+A]
  type AsCollection[+A] <: AsSchema[A]
  type AsPrimitive[+A] <: AsSchema[A]
  type AsTuple[+A] <: AsSchema[A]

  type Schema[A] = Base.Isomorphic[AsSchema, Base.Optional, Base.Schema, ?, A]

  object Schema:
    type Reader[+A] = Base.Reader[AsSchema, Base.Optional, Base.Schema, ?, A]

    type Writer[-A] = Base.Writer[AsSchema, Base.Optional, Base.Schema, ?, A]

  type Collection[A] = Base.Isomorphic[AsCollection, Base.Optional, Base.Collection, ?, A]

  object Collection:
    type Reader[+A] = Base.Reader[AsCollection, Base.Optional, Base.Collection, ?, A]

    type Writer[-A] = Base.Writer[AsCollection, Base.Optional, Base.Collection, ?, A]

  type Primitive[A] = Base.Isomorphic[AsPrimitive, Base.Optional, [_, a] =>> Base.Primitive[a], ?, A]

  object Primitive:
    type Required[A] = Base.Isomorphic[AsPrimitive, Base.Required, [_, a] =>> Base.Primitive[a], ?, A]
