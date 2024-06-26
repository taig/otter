package io.taig.otter

trait SchemaInvariant[Self[_, _], Parent[_, _], Union[_, _]] extends CoproductOps[Self, Parent, Union]:
  extension [A, B](self: Self[A, B]) def union: Union[self.type, B]
