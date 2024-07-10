package io.taig.otter

trait SchemaOps[Self[_, _, _], Optional[_, _, _], Collection[_, _, _], Union[_, _, _], Plain[_, _]]:
  extension [A, B, C](self: Self[A, B, C])
    def collection: Collection[A, self.type, Vector[C]]
    def optional: Optional[A, B, Option[C]]
    def plain: Plain[A, C]
    def union: Union[A, self.type, C]
