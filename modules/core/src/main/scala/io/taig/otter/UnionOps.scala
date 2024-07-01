package io.taig.otter

import cats.Id as Identity

trait UnionOps[Self[_, _], Parent[_, _], Collection[_, _], Plain[_]]
    extends SchemaOps[Self, Self, Collection, Self, Plain],
      CoproductOps[Self, Parent]

object UnionOps:
  trait Isomorphic[Self[_, _], Parent[_, _], Collection[_, _]]
      extends UnionOps[Self, Parent, Collection, Union[Identity, ?, *]],
        SchemaOps.Isomorphic[Self, Self, Collection, Self, Union[Identity, ?, *]]

  trait Reader[Self[_, _], Parent[_, _], Collection[_, _]]
      extends UnionOps[Self, Parent, Collection, Union.Reader[Identity, ?, *]],
        SchemaOps.Reader[Self, Self, Collection, Self, Union.Reader[Identity, ?, *]]

  trait Writer[Self[_, _], Parent[_, _], Collection[_, _]]
      extends UnionOps[Self, Parent, Collection, Union.Writer[Identity, ?, *]],
        SchemaOps.Writer[Self, Self, Collection, Self, Union.Writer[Identity, ?, *]]
