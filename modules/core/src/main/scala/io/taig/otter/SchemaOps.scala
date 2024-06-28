package io.taig.otter

trait SchemaOps[Self[_, _], Optional[_, _], Parent[_, _], Collection[_, _], Union[_, _]]
    extends OrOps[Self, Parent, Union]:
  extension [A, B](self: Self[A, B])
    def collection: Collection[self.type, Vector[B]]
    def optional: Optional[A, Option[B]]
    def union: Union[self.type, B]

object SchemaOps:
  trait Isomorphic[Self[_, _], Optional[_, _], Parent[_, _], Collection[_, _], Union[_, _]]
      extends SchemaOps[Self, Optional, Parent, Collection, Union],
        IsomorphicOps[Self],
        OrOps.Isomorphic[Self, Parent, Union]

  trait Reader[Self[_, _], Optional[_, _], Parent[_, _], Collection[_, _], Union[_, _]]
      extends SchemaOps[Self, Optional, Parent, Collection, Union],
        ReaderOps[Self],
        OrOps.Reader[Self, Parent, Union]:
    extension [A, B](self: Self[A, B]) def asReader: Self[A, B] = self

  trait Writer[Self[_, _], Optional[_, _], Parent[_, _], Collection[_, _], Union[_, _]]
      extends SchemaOps[Self, Optional, Parent, Collection, Union],
        WriterOps[Self],
        OrOps.Writer[Self, Parent, Union]:
    extension [A, B](self: Self[A, B]) def asWriter: Self[A, B] = self
