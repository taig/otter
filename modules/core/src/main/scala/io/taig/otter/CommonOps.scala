package io.taig.otter

trait CommonOps[Self[_, _, _], Collection[_, _, _], Union[_, _, _]]:
  extension [A, B, C](self: Self[A, B, C])
    def collection: Collection[A, self.type, Vector[C]]
    def union: Union[A, self.type, C]
