package io.taig.otter

trait ReaderOps[Self[_, _]]:
  extension [A, B](self: Self[A, B]) def map[C](f: B => C): Self[A, C]
