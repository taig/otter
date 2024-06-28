package io.taig.otter

trait WriterOps[Self[_, _]]:
  extension [A, B](self: Self[A, B]) def contramap[C](f: C => B): Self[A, C]
