package io.taig.otter

import cats.Invariant
import cats.syntax.all.*

trait Tupleable[-F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]]:
  extension [I[a] <: H[a], A](self: F[I, A]) def toTuple: G[I, A]

  extension [I[a] <: H[a], A](self: F[I, A])(using Tuple[G, H])
    final def :*[J[a] >: I[a] <: H[a], B](schema: F[I, B])(using merge: Merge[A, B]): G[J, merge.Out] = ???
    // self.toTuple.zip(schema.toTuple).imap(merge.apply)(merge.unapply)

object Tupleable:
  inline def apply[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using self: Tupleable[F, G, H]): Tupleable[F, G, H] =
    self
