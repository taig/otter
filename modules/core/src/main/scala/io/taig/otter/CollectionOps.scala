package io.taig.otter

import cats.Id as Identity

trait CollectionOps[Self[_, _], Union[_, _], Plain[_], Any] extends SchemaOps[Self, Self, Self, Union, Plain]:
  extension [A, B](self: Self[A, B]) def schema: Any

object CollectionOps:
  trait Isomorphic[Self[_, _], Union[_, _], Plain[a] <: Schema[Identity, ?, a], Any]
      extends CollectionOps[Self, Union, Plain, Any],
        SchemaOps.Isomorphic[Self, Self, Self, Union, Plain]

  trait Reader[Self[_, _], Union[_, _], Plain[a] <: Schema.Reader[Identity, ?, a], Any]
      extends CollectionOps[Self, Union, Plain, Any],
        SchemaOps.Reader[Self, Self, Self, Union, Plain]

  trait Writer[Self[_, _], Union[_, _], Plain[a] <: Schema.Writer[Identity, ?, a], Any]
      extends CollectionOps[Self, Union, Plain, Any],
        SchemaOps.Writer[Self, Self, Self, Union, Plain]
