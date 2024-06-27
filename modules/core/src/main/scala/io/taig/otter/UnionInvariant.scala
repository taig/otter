package io.taig.otter

trait UnionInvariant[Self[_, _], Reader[_, _], Writer[_, _], Parent[_, _], Collection[_, _]]
    extends SchemaInvariant[Self, Reader, Writer, Self, Collection, Self],
      UnionOps[Self, Parent]
