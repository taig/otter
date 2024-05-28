package io.taig.otter

import io.taig.otter as Base

trait Types:
  type AsSchema[+A]
  type AsCollection[+A] <: AsSchema[A]
  type AsPrimitive[+A] <: AsSchema[A]
  type AsTuple[+A] <: AsSchema[A]

  type Parent[D[_[_], _]] = AsSchema[D[Base.Schema[Base.Data[AsSchema, D, ?, *], *], Any]]

  object Parent:
    type Isomorphic = Parent[Base.Isomorphic]
    type Reader = Parent[Base.Reader]
    type Writer = Parent[Base.Writer]

  object Container:
    type Isomorphic[F[_[_], _[_[_], _], _, _], A] =
      Base.Isomorphic[Base.Schema[F[AsSchema, Base.Isomorphic, Parent.Isomorphic, *], *], A]

    type Reader[F[_[_], _[_[_], _], _, _], A] =
      Base.Reader[Base.Schema[F[AsSchema, Base.Reader, Parent.Reader, *], *], A]

    type Writer[F[_[_], _[_[_], _], _, _], A] =
      Base.Writer[Base.Schema[F[AsSchema, Base.Writer, Parent.Writer, *], *], A]

  type Schema[A] = AsSchema[Container.Isomorphic[Base.Data, A]]

  object Schema:
    type Reader[+A] = AsSchema[Container.Reader[Base.Data, A]]

    type Writer[-A] = AsSchema[Container.Writer[Base.Data, A]]

  type Collection[A] = AsCollection[Container.Isomorphic[Base.Collection, A]]

  object Collection:
    type Reader[+A] = AsCollection[Container.Reader[Base.Collection, A]]

    type Writer[-A] = AsCollection[Container.Writer[Base.Collection, A]]

  type Primitive[A] = AsPrimitive[Container.Isomorphic[[_[_], _[_[_], _], _, a] =>> Base.Primitive[a], A]]

  object Primitive:
    type Required[A] = AsPrimitive[Base.Isomorphic[Base.Schema.Required[Base.Primitive, *], A]]
