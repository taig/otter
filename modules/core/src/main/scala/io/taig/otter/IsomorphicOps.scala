package io.taig.otter

trait IsomorphicOps[Self[_, _]]:
  extension [A, B](self: Self[A, B]) def imap[C](f: B => C)(g: C => B): Self[A, C]
