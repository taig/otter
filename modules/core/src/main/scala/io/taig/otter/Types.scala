package io.taig.otter

import io.taig.otter as Base

trait Types:
  type AsSchema[+A]
  type AsCollection[+A] <: AsSchema[A]
  type AsPrimitive[+A] <: AsSchema[A]
  type AsTuple[+A] <: AsSchema[A]

  type Parent[A] = AsSchema[Base.Isomorphic[Base.Schema[[_] =>> A, *], ?]]

  type Schema[A] = AsSchema[Base.Isomorphic[Base.Schema[Base.Data[Parent, *], *], A]]

  type Enumeration[A] = AsSchema[Base.Isomorphic[Base.Schema[Base.Enumeration[Parent, *], *], A]]

  // object Schema:
  //   //   type Of[+A <: AsSchema[Base.Schema[AsSchema, ?, ?, ?]], B] = AsSchema[Base.Schema[AsSchema, Base.Data, A, B]]

  //   type Reader[+A] = AsSchema[Base.Schema.Reader[AsSchema, A]]

  //   type Writer[-A] = AsSchema[Base.Schema.Writer[AsSchema, A]]

  // //   object Writer:
  // //     type Of[+A <: AsSchema[Base.Schema.Writer[AsSchema, ?, ?, ?]], B] =
  // //       AsSchema[Base.Schema.Writer[AsSchema, Base.Data, A, B]]

  // // type Collection[A] = AsCollection[Base.Schema[AsSchema, Base.Collection, ?, A]]

  // type Primitive[A] = AsPrimitive[Base.Schema[AsSchema, A]]

  // object Primitive:
  //   type Required[A] = AsPrimitive[Base.Schema.Required[AsSchema, A]]

  //   object Required:
  //     type Reader[+A] = AsPrimitive[Base.Schema.Required.Reader[AsSchema, A]]

  //     type Writer[-A] = AsPrimitive[Base.Schema.Required.Writer[AsSchema, A]]

  // // type Tuple[A] = AsSchema[Base.Schema[AsSchema, Base.Tuple, ?, A]]

  // // object Tuple:
  // //   type Reader[+A] = AsSchema[Base.Schema.Reader[AsSchema, Base.Tuple, ?, A]]

  // //   type Writer[-A] = AsSchema[Base.Schema.Writer[AsSchema, Base.Tuple, ?, A]]
