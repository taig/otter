package io.taig.otter

import io.taig.otter as Base

trait Types:
  type AsSchema[A]
  type AsCollection[A] <: AsSchema[A]
  type AsPrimitive[A] <: AsSchema[A]
  type AsTuple[A] <: AsSchema[A]

  type Isomorphic[F[a] <: AsSchema[a], O[_[_], _], S[_, _], A <: Base.Isomorphic.Any[AsSchema], B] =
    Base.Isomorphic[F, AsSchema, O, S, A, B]

  object Isomorphic:
    type Any = Base.Isomorphic.Any[AsSchema]

  type Schema[A] = Isomorphic[AsSchema, Base.Optional, Base.Schema, ?, A]

  object Schema:
    type Of[A <: Isomorphic.Any, B] = Isomorphic[AsSchema, Base.Optional, Base.Schema, A, B]

    type Reader[+A] = Base.Reader[AsSchema, AsSchema, Base.Optional, Base.Schema, ?, A]

    object Reader:
      type Of[A <: Base.Reader.Any[AsSchema], B] = Base.Reader[AsSchema, AsSchema, Base.Optional, Base.Schema, A, B]

    type Writer[-A] = Base.Writer[AsSchema, AsSchema, Base.Optional, Base.Schema, ?, A]

    object Writer:
      type Of[A <: Base.Writer.Any[AsSchema], B] = Base.Writer[AsSchema, AsSchema, Base.Optional, Base.Schema, A, B]

  type Collection[A] = Isomorphic[AsCollection, Base.Optional, Base.Collection, ?, A]

  object Collection:
    type Of[A <: Isomorphic.Any, B] = Isomorphic[AsCollection, Base.Optional, Base.Collection, A, B]

    type Reader[+A] = Base.Reader[AsCollection, AsSchema, Base.Optional, Base.Collection, ?, A]

    type Writer[-A] = Base.Writer[AsCollection, AsSchema, Base.Optional, Base.Collection, ?, A]

  type Primitive[A] = Isomorphic[AsPrimitive, Base.Optional, [_, a] =>> Base.Primitive[a], ?, A]

  object Primitive:
    type Required[A] = Isomorphic[AsPrimitive, Base.Required, [_, a] =>> Base.Primitive[a], ?, A]
