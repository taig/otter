package io.taig.otter

import cats.Invariant
import cats.syntax.all.*

trait Field[F[+_[a] <: H[a], a], G[+_[a] <: H[a], _], H[_]]:
  def field[I[a] <: H[a], A](name: String, schema: Reference[I, A]): F[I, A]

  extension [I[a] <: H[a], A](self: F[I, A]) def toRecord: G[I, A]

  // extension [I[a] <: H[a], A](self: F[I, A])(using Record[G, F, H])
  //   final def :*[J[a] >: I[a] <: H[a], B](schema: F[J, B])(using
  //       merge: Merge[A, B]
  //   )(using Invariant[G[J, *]]): G[J, merge.Out] =
  //     self.toRecord.zip(schema.toRecord).imap(merge.apply)(merge.unapply)

  //   final def *:[J[a] >: I[a] <: H[a], B](schema: F[J, B])(using
  //       merge: Merge[A, B]
  //   )(using Invariant[G[J, *]]): G[J, merge.Out] =
  //     self.toRecord.zip(schema.toRecord).imap(merge.apply)(merge.unapply)

object Field:
  // TODO imapK?
  sealed trait Read[F[+_[a] <: H[a], a] <: H[a], G[+_[a] <: H[a], _], H[_]] extends Field[F, G, H]

  sealed trait Write[F[+_[a] <: H[a], a] <: H[a], G[+_[a] <: H[a], _], H[_]] extends Field[F, G, H]

  inline def apply[F[+_[a] <: H[a], a] <: H[a], G[+_[a] <: H[a], _], H[_]](using
      self: Field[F, G, H]
  ): Field[F, G, H] = self
