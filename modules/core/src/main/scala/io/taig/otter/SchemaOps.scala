package io.taig.otter

trait SchemaOps[Self[_, _], Optional[_, _], Collection[_, _], Union[_, _]]:
  extension [A, B](self: Self[A, B])
    def collection: Collection[self.type, Vector[B]]
    def optional: Optional[A, Option[B]]
    def union: Union[self.type, B]

object SchemaOps:
  trait Isomorphic[Self[_, _], Optional[_, _], Collection[_, _], Union[_, _]]
      extends SchemaOps[Self, Optional, Collection, Union]:
    extension [A, B](self: Self[A, B]) def imap[C](f: B => C)(g: C => B): Self[A, C]

  trait Reader[Self[_, _], Optional[_, _], Collection[_, _], Union[_, _]]
      extends SchemaOps[Self, Optional, Collection, Union]:
    extension [A, B](self: Self[A, B])
      def asReader: Self[A, B] = self
      def map[C](f: B => C): Self[A, C]

  trait Writer[Self[_, _], Optional[_, _], Collection[_, _], Union[_, _]]
      extends SchemaOps[Self, Optional, Collection, Union]:
    extension [A, B](self: Self[A, B])
      def asWriter: Self[A, B] = self
      def contramap[C](f: C => B): Self[A, C]
