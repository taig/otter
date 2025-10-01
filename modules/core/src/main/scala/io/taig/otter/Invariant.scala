package io.taig.otter

import cats.Invariant as CatsInvariant

trait Invariant[F[_]]:
  self =>

  final def F: CatsInvariant[F] = new CatsInvariant[F]:
    override def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] = self.imap(fa)(f)(g)

  extension [A](self: F[A]) def imap[B](f: A => B)(g: B => A): F[B]

  def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): Invariant[G] =
    new Invariant[G]:
      extension [A](ga: G[A])
        override def imap[B](f: A => B)(g: B => A): G[B] =
          fK(self.imap(gK(ga))(f)(g))

object Invariant:
  inline def apply[F[_]](using invariant: Invariant[F]): Invariant[F] = invariant
