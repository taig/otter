package io.taig.otter

import io.taig.otter as Base

trait Types:
  type AsSchema[+A]
  type AsCollection[+A] <: AsSchema[A]
  type AsPrimitive[+A] <: AsSchema[A]
  type AsTuple[+A] <: AsSchema[A]

  type Schema[A] = Wrapper[
    AsSchema,
    Base.Isomorphic,
    Base.Optional,
    [f[_], d[_[_], _], a] =>> Base.Schema[f, d, ?, a],
    A
  ]

  object Schema:
    type Reader[+A] = Wrapper[
      AsSchema,
      Base.Reader,
      Base.Optional,
      [f[_], d[_[_], _], a] =>> Base.Schema[f, d, ?, a],
      A
    ]

    type Writer[-A] = Wrapper[
      AsSchema,
      Base.Writer,
      Base.Optional,
      [f[_], d[_[_], _], a] =>> Base.Schema[f, d, ?, a],
      A
    ]

  // type Collection[A] = AsCollection[Container.Isomorphic[Base.Collection, A]]

  // object Collection:
  //   type Reader[+A] = AsCollection[Container.Reader[Base.Collection, A]]

  //   type Writer[-A] = AsCollection[Container.Writer[Base.Collection, A]]

  // type Primitive[A] = AsPrimitive[Container.Isomorphic[[_[_], _[_[_], _], _, a] =>> Base.Primitive[a], A]]

  object Primitive:
    type Required[A] = AsPrimitive[Base.Isomorphic[Base.Required[Base.Primitive, *], A]]
