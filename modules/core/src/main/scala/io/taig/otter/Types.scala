package io.taig.otter

import io.taig.otter as Base

trait Types:
  self =>

  type AsSchema[+A]
  type AsCollection[+A] <: AsSchema[A]
  type AsPrimitive[+A] <: AsSchema[A]
  type AsTuple[+A] <: AsSchema[A]

  // type Schema[A] = AsSchema[Base.Optional[AsSchema, Base.Isomorphic, Base.Schema, ?, A]]

  // object Schema:
  //   type Of[A <: AsSchema[Optional[AsSchema, Base.Isomorphic, Base.Schema, ?, ?]], B] =
  //     AsSchema[Base.Optional[AsSchema, Base.Isomorphic, Base.Schema, A, B]]

  //   type Writer[A] = AsSchema[Base.Optional[AsSchema, Base.Writer, Base.Schema, ?, A]]

  // type Isomorphic[A] = AsSchema[Base.Isomorphic[AsSchema, Base.Optional, Base.Schema, ?, A]]

  // object Isomorphic:
  //   type Any = AsSchema[Base.Isomorphic[AsSchema, Base.Optional, Base.Schema, ?, ?]]

  // type Reader[A] = AsSchema[Base.Reader[AsSchema, Base.Optional, Base.Schema, ?, A]]

  // object Reader:
  //   type Any = AsSchema[Base.Reader[AsSchema, Base.Optional, Base.Schema, ?, ?]]

  // type Writer[A] = AsSchema[Base.Writer[AsSchema, Base.Optional, Base.Schema, ?, A]]

  // object Writer:
  //   type Any = AsSchema[Base.Writer[AsSchema, Base.Optional, Base.Schema, ?, ?]]

  // type Schema[A] = Schema.Of[self.Isomorphic.Any, A]

  // object Schema:
  //   type Of[A <: self.Isomorphic.Any, B] = AsSchema[Base.Isomorphic[AsSchema, Base.Optional, Base.Schema, A, B]]

  //   type Reader[+A] = Reader.Of[self.Reader.Any, A]

  //   object Reader:
  //     type Of[A <: self.Reader.Any, B] = AsSchema[Base.Reader[AsSchema, Base.Optional, Base.Schema, A, B]]

  //   type Writer[-A] = Writer.Of[self.Writer.Any, A]

  //   object Writer:
  //     type Of[A <: self.Writer.Any, B] = AsSchema[Base.Writer[AsSchema, Base.Optional, Base.Schema, A, B]]

  // type Collection[A] = AsCollection[Base.Isomorphic[AsSchema, Base.Optional, Base.Collection, self.Isomorphic.Any, A]]

  // object Collection:
  //   type Of[A <: self.Isomorphic.Any, B] =
  //     AsCollection[Base.Isomorphic[AsSchema, Base.Optional, Base.Collection, A, B]]

  //   type Reader[+A] = AsCollection[Base.Reader[AsSchema, Base.Optional, Base.Collection, self.Reader.Any, A]]

  //   object Reader:
  //     type Of[A <: self.Reader.Any, B] = AsCollection[Base.Reader[AsSchema, Base.Optional, Base.Collection, A, B]]

  //   type Writer[-A] = AsCollection[Base.Writer[AsSchema, Base.Optional, Base.Collection, self.Writer.Any, A]]

  //   object Writer:
  //     type Of[A <: self.Writer.Any, B] = AsCollection[Base.Writer[AsSchema, Base.Optional, Base.Collection, A, B]]

  // type Primitive[A] = AsPrimitive[Base.Isomorphic[AsSchema, Base.Optional, [_, a] =>> Base.Primitive[a], Nothing, A]]

  // object Primitive:
  //   type Required[A] = AsPrimitive[Base.Isomorphic[AsSchema, Base.Required, [_, a] =>> Base.Primitive[a], Nothing, A]]

  // type Tuple[A] = AsTuple[Base.Isomorphic[AsSchema, Base.Optional, Base.Tuple, self.Isomorphic.Any, A]]

  // object Tuple:
  //   type Of[A <: self.Isomorphic.Any, B] = AsTuple[Base.Isomorphic[AsSchema, Base.Optional, Base.Tuple, A, B]]

  //   type Reader[+A] = AsTuple[Base.Reader[AsSchema, Base.Optional, Base.Tuple, self.Reader.Any, A]]

  //   object Reader:
  //     type Of[A <: self.Reader.Any, B] = AsTuple[Base.Reader[AsSchema, Base.Optional, Base.Tuple, A, B]]

  //   type Writer[-A] = AsTuple[Base.Writer[AsSchema, Base.Optional, Base.Tuple, self.Writer.Any, A]]

  //   object Writer:
  //     type Of[A <: self.Writer.Any, B] = AsTuple[Base.Writer[AsSchema, Base.Optional, Base.Tuple, A, B]]
