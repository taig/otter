package io.taig.otter

trait SchemaOps[Self[_, _], Optional[_, _], Collection[_, _], Union[_, _]]:
  extension [A, B](self: Self[A, B])
    def collection: Collection[A, B]
    def optional: Optional[A, Option[B]]
    def union: Union[A, B]
