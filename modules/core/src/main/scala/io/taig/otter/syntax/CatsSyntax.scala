package io.taig.otter.syntax

import cats.Contravariant
import cats.Functor
import cats.Invariant

trait CatsSyntax:
  extension [F[_]](fa: Contravariant[F])
    def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): Contravariant[G] = new Contravariant[G]:
      override def contramap[A, B](ga: G[A])(f: B => A): G[B] = fK(fa.contramap(gK(ga))(f))

  extension [F[_]](fa: Functor[F])
    def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): Functor[G] = new Functor[G]:
      override def map[A, B](ga: G[A])(f: A => B): G[B] = fK(fa.map(gK(ga))(f))

  extension [F[_]](fa: Invariant[F])
    def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): Invariant[G] = new Invariant[G]:
      override def imap[A, B](ga: G[A])(f: A => B)(g: B => A): G[B] = fK(fa.imap(gK(ga))(f)(g))

object CatsSyntax extends CatsSyntax
