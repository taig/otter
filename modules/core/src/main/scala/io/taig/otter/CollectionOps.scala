package io.taig.otter

trait CollectionOps[Self[_, _], Union[_, _], Any] extends SchemaOps[Self, Self, Self, Union]:
  extension [A, B](self: Self[A, B]) def schema: Any

object CollectionOps:
  trait Isomorphic[Self[_, _], Union[_, _], Writer[_], Any]
      extends CollectionOps[Self, Union, Any],
        SchemaOps.Isomorphic[Self, Self, Self, Union]

  trait Reader[Self[_, _], Union[_, _], Writer[_], Any]
      extends CollectionOps[Self, Union, Any],
        SchemaOps.Reader[Self, Self, Self, Union]

  trait Writer[Self[_, _], Union[_, _], Writer[_], Any]
      extends CollectionOps[Self, Union, Any],
        SchemaOps.Writer[Self, Self, Self, Union]
