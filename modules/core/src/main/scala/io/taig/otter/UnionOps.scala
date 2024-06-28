package io.taig.otter

trait UnionOps[Self[_, _], Parent[_, _], Collection[_, _]]
    extends SchemaOps[Self, Self, Parent, Collection, Self],
      OrElseOps[Self],
      OrOps[Self, Parent, Self]

object UnionOps:
  trait Isomorphic[Self[_, _], Parent[_, _], Collection[_, _]]
      extends UnionOps[Self, Parent, Collection],
        SchemaOps.Isomorphic[Self, Self, Parent, Collection, Self],
        OrElseOps.Isomorphic[Self],
        OrOps.Isomorphic[Self, Parent, Self]

  trait Reader[Self[_, _], Parent[_, _], Collection[_, _]]
      extends UnionOps[Self, Parent, Collection],
        SchemaOps.Reader[Self, Self, Parent, Collection, Self],
        OrElseOps.Reader[Self],
        OrOps.Reader[Self, Parent, Self]

  trait Writer[Self[_, _], Parent[_, _], Collection[_, _]]
      extends UnionOps[Self, Parent, Collection],
        SchemaOps.Writer[Self, Self, Parent, Collection, Self],
        OrElseOps.Writer[Self],
        OrOps.Writer[Self, Parent, Self]
