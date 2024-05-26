package io.taig.otter

import io.taig.otter as Base

trait Types:
  type AsSchema[+A]
  type AsCollection[+A] <: AsSchema[A]
  type AsPrimitive[+A] <: AsSchema[A]
  type AsTuple[+A] <: AsSchema[A]

  private type Parent[D[_[_], _]] = AsSchema[D[Base.Schema[Base.Data[AsSchema, D, ?, *], *], Any]]

  type Schema[A] =
    AsSchema[Base.Isomorphic[Base.Schema[Base.Data[AsSchema, Base.Isomorphic, Parent[Base.Isomorphic], *], *], A]]

  object Schema:
    type Reader[+A] =
      AsSchema[Base.Reader[Base.Schema[Base.Data[AsSchema, Base.Reader, Parent[Base.Isomorphic], *], *], A]]

    type Writer[-A] =
      AsSchema[Base.Writer[Base.Schema[Base.Data[AsSchema, Base.Writer, Parent[Base.Isomorphic], *], *], A]]

  type Collection[A] =
    AsCollection[Base.Isomorphic[Base.Schema[Base.Collection[AsSchema, Base.Isomorphic, ?, *], *], A]]

  type Primitive[A] = AsPrimitive[Base.Isomorphic[Base.Schema[Base.Primitive, *], A]]

  object Primitive:
    type Required[A] = AsPrimitive[Base.Isomorphic[Base.Schema.Required[Base.Primitive, *], A]]
