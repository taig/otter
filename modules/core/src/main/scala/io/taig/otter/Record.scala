package io.taig.otter

import cats.data.Chain
import cats.Invariant
import cats.syntax.all.*

trait Record[F[_[+_[_], _], +_[_], _], G[+_[_], _], H[_]]:
  self =>

  def apply[I[a] <: H[a], A](field: Reference[G[I, *], A]): F[G, I, A]

  def empty: F[G, H, Unit]

  extension [J[a] <: H[a], A](fgja: F[G, J, A])
    def fields: Chain[Reference[G[J, *], ?]]

    def zip[K[a] >: J[a] <: H[a], B](schema: F[G, K, B]): F[G, K, (A, B)]

    final def *[K[a] >: J[a] <: H[a], B](field: G[K, B])(using
        merge: Merge[A, B]
    )(using Invariant[F[G, K, *]]): F[G, K, merge.Out] =
      self.zip(fgja)(self.apply(Reference.now(field))).imap(merge.apply)(merge.unapply)

    final def :*[K[a] >: J[a] <: H[a], B](field: G[K, B])(using
        append: Append[A, B]
    )(using Invariant[F[G, K, *]]): F[G, K, append.Out] =
      self.zip(fgja)(self.apply(Reference.now(field))).imap(append.apply)(append.unapply)

    final def *:[K[a] >: J[a] <: H[a], B](field: G[K, B])(using
        prepend: Prepend[A, B]
    )(using Invariant[F[G, K, *]]): F[G, K, prepend.Out] =
      self.zip(fgja)(self.apply(Reference.now(field))).imap(prepend.apply)(prepend.unapply)

  def imapK[I[_[+_[_], _], +_[_], _]](fK: [J[+_[_], _], K[a] <: H[a], A] => F[J, K, A] => I[J, K, A])(
      gK: [J[+_[_], _], K[a] <: H[a], A] => I[J, K, A] => F[J, K, A]
  ): Record[I, G, H] = new Record[I, G, H]:
    override def apply[J[a] <: H[a], A](field: Reference[G[J, *], A]): I[G, J, A] = fK(self.apply(field))

    override def empty: I[G, H, Unit] = fK(self.empty)

    extension [J[a] <: H[a], A](igja: I[G, J, A])
      override def fields: Chain[Reference[G[J, *], ?]] = self.fields(gK(igja))

      override def zip[K[a] >: J[a] <: H[a], B](schema: I[G, K, B]): I[G, K, (A, B)] =
        fK(self.zip(gK(igja))(gK(schema)))

object Record:
  given InvariantK3[Record] with
    extension [G[_[+_[_], _], +_[_], _], H[+_[_], _], I[_]](fa: Record[G, H, I])
      def imapK[J[_[+_[_], _], +_[_], _]](fK: [K[+_[_], _], L[a] <: I[a], A] => G[K, L, A] => J[K, L, A])(
          gK: [K[+_[_], _], L[a] <: I[a], A] => J[K, L, A] => G[K, L, A]
      ): Record[J, H, I] = fa.imapK(fK)(gK)
