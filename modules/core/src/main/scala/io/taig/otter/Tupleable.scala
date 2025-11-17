package io.taig.otter

import cats.syntax.all.*
import cats.Invariant
import scala.annotation.targetName

trait Tupleable[F[a] <: H[a], G[+_[a] <: H[a], a] <: H[a], H[_]]:
  extension [A](self: F[A]) def toTuple: G[F, A]

  extension [A](self: F[A])(using Tuple[G, H])
    final def :*[X[a] <: H[a], J[a] <: H[a], B](schema: X[B])(using
        merge: Merge[A, B]
    )(using Tupleable[X, G, H], Invariant[G[[a] =>> (F[a] | X[a]), *]]): G[[a] =>> (F[a] | X[a]), merge.Out] =
      self.toTuple.zip(schema.toTuple).imap(merge.apply)(merge.unapply)
