package io.taig.otter

import io.taig.otter as Base

trait Types:
  type AsSchema[+A]
  type AsCollection[+A] <: AsSchema[A]
  type AsPrimitive[+A] <: AsSchema[A]
  type AsTuple[+A] <: AsSchema[A]

  object Parent:
    type Isomorphic[A] = AsSchema[Base.Isomorphic[Base.Optional, Base.Schema, Isomorphic.Any, A]]

    object Isomorphic:
      type Any = AsSchema[Base.Isomorphic[Base.Optional, Base.Schema, ?, ?]]

    type Reader[A] = AsSchema[Base.Reader[Base.Optional, Base.Schema, Reader.Any, A]]

    object Reader:
      type Any = AsSchema[Base.Reader[Base.Optional, Base.Schema, ?, ?]]

    type Writer[A] = AsSchema[Base.Writer[Base.Optional, Base.Schema, Writer.Any, A]]

    object Writer:
      type Any = AsSchema[Base.Writer[Base.Optional, Base.Schema, ?, ?]]

  type Schema[A] = Schema.Of[Parent.Isomorphic.Any, A]

  object Schema:
    type Of[A <: Parent.Isomorphic.Any, B] = AsSchema[Base.Isomorphic[Base.Optional, Base.Schema, A, B]]

    type Reader[+A] = Reader.Of[Parent.Reader.Any, A]

    object Reader:
      type Of[A <: Parent.Reader.Any, B] = AsSchema[Base.Reader[Base.Optional, Base.Schema, A, B]]

    type Writer[-A] = Writer.Of[Parent.Writer.Any, A]

    object Writer:
      type Of[A <: Parent.Writer.Any, B] = AsSchema[Base.Writer[Base.Optional, Base.Schema, A, B]]

  type Collection[A] = AsCollection[Base.Isomorphic[Base.Optional, Base.Collection, Parent.Isomorphic.Any, A]]

  object Collection:
    type Of[A <: Parent.Isomorphic.Any, B] = AsCollection[Base.Isomorphic[Base.Optional, Base.Collection, A, B]]

  //   type Reader[+A] = Base.Reader[AsCollection, Base.Optional, Base.Collection, ?, A]

  //   type Writer[-A] = Base.Writer[AsCollection, Base.Optional, Base.Collection, ?, A]

  type Primitive[A] = AsPrimitive[Base.Isomorphic[Base.Optional, [_, a] =>> Base.Primitive[a], Nothing, A]]

  object Primitive:
    type Required[A] = AsPrimitive[Base.Isomorphic[Base.Required, [_, a] =>> Base.Primitive[a], Nothing, A]]
