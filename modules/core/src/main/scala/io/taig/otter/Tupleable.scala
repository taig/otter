package io.taig.otter

import cats.Invariant
import cats.syntax.all.*

trait Tupleable[F[+_[a] <: H[a], a] <: H[a], G[+_[a] <: H[a], _], H[_]]:
  extension [I[a] <: H[a], A](self: F[I, A]) def toTuple: G[F[I, *], A]

  extension [I[a] <: H[a], A](self: F[I, A])(using Tuple[G, F[I, *]], Invariant[G[F[I, *], *]])
    final def :*[B](schema: F[I, B])(using merge: Merge[A, B]): G[F[I, *], merge.Out] =
      self.toTuple.zip(schema.toTuple).imap(merge.apply)(merge.unapply)
