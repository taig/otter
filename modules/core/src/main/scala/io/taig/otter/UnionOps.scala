package io.taig.otter

trait UnionOps[Self[_, _], Parent[_, _], Collection[_, _]]
    extends SchemaOps[Self, Self, Collection, Self],
      CoproductOps[Self, Parent]

object UnionOps:
  trait Isomorphic[Self[_, _], Parent[_, _], Collection[_, _]]
      extends UnionOps[Self, Parent, Collection],
        SchemaOps.Isomorphic[Self, Self, Collection, Self],
        CoproductOps.Isomorphic[Self, Parent]

  trait Reader[Self[_, _], Parent[_, _], Collection[_, _]]
      extends UnionOps[Self, Parent, Collection],
        SchemaOps.Reader[Self, Self, Collection, Self],
        CoproductOps.Reader[Self, Parent]

  trait Writer[Self[_, _], Parent[_, _], Collection[_, _]]
      extends UnionOps[Self, Parent, Collection],
        SchemaOps.Writer[Self, Self, Collection, Self],
        CoproductOps.Writer[Self, Parent]
