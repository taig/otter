package io.taig.otter

import cats.Invariant

trait SchemaInvariant[Self[_, _], Reader[_, _], Writer[_, _], Optional[_, _], Collection[_, _], Union[_, _]]
    extends SchemaOps[Self, Optional, Collection, Union]:
  given invariant[A]: Invariant[Self[A, *]]

  extension [A, B](self: Self[A, B])
    def asReader: Reader[A, B]
    def asWriter: Writer[A, B]
