package io.taig.otter


trait Tupleable[F[+_[a] <: H[a], a] <: H[a], G[+_[a] <: H[a], _], H[_]]:
  extension [I[a] <: H[a], A](self: F[I, A]) def toTuple: G[F[I, *], A]

  extension [I[a] <: H[a], A](self: F[I, A]) // (using Tuple[G, F[H, *]])
    final def :*[X[a] <: H[a], B](schema: X[B])(using
        merge: Merge[A, B]
    ): G[[a] =>> (F[I, a] | X[a]), merge.Out] = ???
    // self.toTuple.zip(schema.toTuple).imap(merge.apply)(merge.unapply)
