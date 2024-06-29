package io.taig.otter

trait UnionOps[Self[_, _], Parent[_, _], Collection[_, _]]
    extends SchemaOps[Self, Self, Parent, Collection, Self],
      CoproductOps[Self]

object UnionOps:
  trait Isomorphic[Self[_, _], Parent[_, _], Collection[_, _]]
      extends UnionOps[Self, Parent, Collection],
        SchemaOps.Isomorphic[Self, Self, Parent, Collection, Self],
        CoproductOps.Isomorphic[Self]

  trait Reader[Self[_, _], Parent[_, _], Collection[_, _]]
      extends UnionOps[Self, Parent, Collection],
        SchemaOps.Reader[Self, Self, Parent, Collection, Self],
        CoproductOps.Reader[Self]

  trait Writer[Self[_, _], Parent[_, _], Collection[_, _]]
      extends UnionOps[Self, Parent, Collection],
        SchemaOps.Writer[Self, Self, Parent, Collection, Self],
        CoproductOps.Writer[Self]
