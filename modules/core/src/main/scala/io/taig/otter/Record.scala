package io.taig.otter

import cats.data.Chain
import cats.Invariant
import cats.syntax.all.*

trait Record[F[+_[a] <: H[a], _], G[+_[_], _], H[_]]:
  self =>

  def apply[I[a] <: H[a], A](field: Reference[G[I, *], A]): F[I, A]

  def empty: F[Nothing, Unit]

  def imapK[I[+_[a] <: H[a], _]](fK: [S[a] <: H[a], A] => F[S, A] => I[S, A])(
      gK: [S[a] <: H[a], A] => I[S, A] => F[S, A]
  ): Record[I, G, H] = new Record[I, G, H]:
    override def apply[J[a] <: H[a], A](field: Reference[G[J, *], A]): I[J, A] = fK(self.apply(field))

    override def empty: I[Nothing, Unit] = fK(self.empty)

    extension [J[a] <: H[a], A](ija: I[J, A])
      override def fields: Chain[Reference[G[J, *], ?]] = self.fields(gK(ija))

      override def zip[K[a] >: J[a] <: H[a], B](schema: I[K, B]): I[K, (A, B)] =
        fK(self.zip(gK(ija))(gK(schema)))

  extension [I[a] <: H[a], A](fia: F[I, A])
    def fields: Chain[Reference[G[I, *], ?]]

    def zip[J[a] >: I[a] <: H[a], B](schema: F[J, B]): F[J, (A, B)]

    final def :*[J[a] >: I[a] <: H[a], B](field: G[J, B])(using
        append: Append[A, B]
    )(using Invariant[F[J, *]]): F[J, append.Out] = self
      .zip(fia)(self.apply(Reference.now(field)))
      .imap(append.apply)(append.unapply)

object Record:
  given [G[+_[_], _], H[_]]: InvariantK3[[f[+_[a] <: h[a], _], g[+_[a], _], h[_]] =>> Record[f, g, h]] with
    extension [F[+_[a] <: H1[a], _], G1[+_[a], _], H1[_]](fa: Record[F, G1, H1])
      def imapK[I[+_[a] <: H1[a], _]](fK: [S[a] <: H1[a], A] => F[S, A] => I[S, A])(
          gK: [S[a] <: H1[a], A] => I[S, A] => F[S, A]
      ): Record[I, G1, H1] = fa.imapK(fK)(gK)
