package io.taig.otter

import io.taig.otter as Base

trait Types:
  type AsSchema[+A]
  type AsCollection[+A] <: AsSchema[A]
  type AsPrimitive[+A] <: AsSchema[A]
  type AsTuple[+A] <: AsSchema[A]

  object Parent:
    type Isomorphic[A] = AsSchema[Base.Isomorphic[AsSchema, Base.Optional, Base.Schema, ?, A]]

    object Isomorphic:
      type Any = AsSchema[Base.Isomorphic[AsSchema, Base.Optional, Base.Schema, ?, ?]]

    type Reader[A] = AsSchema[Base.Reader[AsSchema, Base.Optional, Base.Schema, ?, A]]

    object Reader:
      type Any = AsSchema[Base.Reader[AsSchema, Base.Optional, Base.Schema, ?, ?]]

    type Writer[A] = AsSchema[Base.Writer[AsSchema, Base.Optional, Base.Schema, ?, A]]

    object Writer:
      type Any = AsSchema[Base.Writer[AsSchema, Base.Optional, Base.Schema, ?, ?]]

  type Schema[A] = Schema.Of[Parent.Isomorphic.Any, A]

  object Schema:
    type Of[A <: Parent.Isomorphic.Any, B] = AsSchema[Base.Isomorphic[AsSchema, Base.Optional, Base.Schema, A, B]]

    type Reader[+A] = Reader.Of[Parent.Reader.Any, A]

    object Reader:
      type Of[A <: Parent.Reader.Any, B] = AsSchema[Base.Reader[AsSchema, Base.Optional, Base.Schema, A, B]]

    type Writer[-A] = Writer.Of[Parent.Writer.Any, A]

    object Writer:
      type Of[A <: Parent.Writer.Any, B] = AsSchema[Base.Writer[AsSchema, Base.Optional, Base.Schema, A, B]]

  type Collection[A] = AsCollection[Base.Isomorphic[AsSchema, Base.Optional, Base.Collection, Parent.Isomorphic.Any, A]]

  object Collection:
    type Of[A <: Parent.Isomorphic.Any, B] =
      AsCollection[Base.Isomorphic[AsSchema, Base.Optional, Base.Collection, A, B]]

    type Reader[+A] = AsCollection[Base.Reader[AsSchema, Base.Optional, Base.Collection, Parent.Reader.Any, A]]

    object Reader:
      type Of[A <: Parent.Reader.Any, B] = AsCollection[Base.Reader[AsSchema, Base.Optional, Base.Collection, A, B]]

    type Writer[-A] = AsCollection[Base.Writer[AsSchema, Base.Optional, Base.Collection, Parent.Writer.Any, A]]

    object Writer:
      type Of[A <: Parent.Writer.Any, B] = AsCollection[Base.Writer[AsSchema, Base.Optional, Base.Collection, A, B]]

  type Primitive[A] = AsPrimitive[Base.Isomorphic[AsSchema, Base.Optional, [_, a] =>> Base.Primitive[a], Nothing, A]]

  object Primitive:
    type Required[A] = AsPrimitive[Base.Isomorphic[AsSchema, Base.Required, [_, a] =>> Base.Primitive[a], Nothing, A]]

  type Tuple[A] = AsTuple[Base.Isomorphic[AsSchema, Base.Optional, Base.Tuple, Parent.Isomorphic.Any, A]]

  object Tuple:
    type Of[A <: Parent.Isomorphic.Any, B] = AsTuple[Base.Isomorphic[AsSchema, Base.Optional, Base.Tuple, A, B]]

    type Reader[+A] = AsTuple[Base.Reader[AsSchema, Base.Optional, Base.Tuple, Parent.Reader.Any, A]]

    type Writer[-A] = AsTuple[Base.Writer[AsSchema, Base.Optional, Base.Tuple, Parent.Writer.Any, A]]
