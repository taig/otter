package io.taig.otter

import cats.data.Chain
import cats.Invariant
import cats.syntax.all.*

trait Record[F[+_[a] <: G[a], _], G[_]]:
  self =>

  def apply[I[a] <: G[a], A](field: Reference[I, A]): F[I, A]

  def empty: F[Nothing, Unit]

  // extension [I[a] <: H[a], A](fia: F[I, A])
  //   def fields: Chain[Reference[G[I, *], ?]]

  //   def zip[J[a] >: I[a] <: H[a], B](schema: F[J, B]): F[J, (A, B)]

  // final def :*[J[a] >: I[a] <: H[a], B](field: G[J, B])(using
  //     append: Append[A, B]
  // )(using Invariant[F[J, *]]): F[J, append.Out] = self
  //   .zip(fia)(self.apply(Reference.now(field)))
  //   .imap(append.apply)(append.unapply)
