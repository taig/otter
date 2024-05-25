package io.taig.otter

import io.taig.otter as Base
import cats.Id

trait Types:
  type AsSchema[+A]
  type AsCollection[+A] <: AsSchema[A]
  type AsPrimitive[+A] <: AsSchema[A]
  type AsTuple[+A] <: AsSchema[A]

  type Schema[A] = AsSchema[Base.Isomorphic[Base.Schema[Base.Data[AsSchema, Base.Isomorphic, *], *], A]]
  type Collection[A] = AsCollection[Base.Isomorphic[Base.Schema[Base.Collection[AsSchema, Base.Isomorphic, *], *], A]]
