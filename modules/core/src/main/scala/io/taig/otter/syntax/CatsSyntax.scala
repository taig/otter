package io.taig.otter.syntax

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.InvariantSemigroupal
import cats.Apply
import cats.ContravariantSemigroupal

trait CatsSyntax:
  extension [F[_]](fa: Apply[F])
    def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): Apply[G] = new Apply[G]:
      override def map[A, B](ga: G[A])(f: A => B): G[B] = fK(fa.map(gK(ga))(f))
      override def ap[A, B](ff: G[A => B])(ga: G[A]): G[B] = fK(fa.ap(gK(ff))(gK(ga)))

  extension [F[_]](fa: Contravariant[F])
    def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): Contravariant[G] = new Contravariant[G]:
      override def contramap[A, B](ga: G[A])(f: B => A): G[B] = fK(fa.contramap(gK(ga))(f))

  extension [F[_]](fa: ContravariantSemigroupal[F])
    def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): ContravariantSemigroupal[G] =
      new ContravariantSemigroupal[G]:
        override def contramap[A, B](ga: G[A])(f: B => A): G[B] = fK(fa.contramap(gK(ga))(f))
        override def product[A, B](ga: G[A], gb: G[B]): G[(A, B)] = fK(fa.product(gK(ga), gK(gb)))

  extension [F[_]](fa: Functor[F])
    def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): Functor[G] = new Functor[G]:
      override def map[A, B](ga: G[A])(f: A => B): G[B] = fK(fa.map(gK(ga))(f))

  extension [F[_]](fa: Invariant[F])
    def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): Invariant[G] = new Invariant[G]:
      override def imap[A, B](ga: G[A])(f: A => B)(g: B => A): G[B] = fK(fa.imap(gK(ga))(f)(g))

  extension [F[_]](fa: InvariantSemigroupal[F])
    def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): InvariantSemigroupal[G] =
      new InvariantSemigroupal[G]:
        override def imap[A, B](ga: G[A])(f: A => B)(g: B => A): G[B] = fK(fa.imap(gK(ga))(f)(g))
        override def product[A, B](ga: G[A], gb: G[B]): G[(A, B)] = fK(fa.product(gK(ga), gK(gb)))

object CatsSyntax extends CatsSyntax
