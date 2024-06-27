package io.taig.otter

trait UnionOps[Self[_, _], Parent[_, _]] extends CoproductOps[Self, Parent, Self]
