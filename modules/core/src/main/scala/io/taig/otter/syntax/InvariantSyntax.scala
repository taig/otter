package io.taig.otter.syntax

import cats.Invariant

trait InvariantSyntax:
  extension [F[_]](fa: Invariant[F])
    def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): Invariant[G] = new Invariant[G]:
      override def imap[A, B](ga: G[A])(f: A => B)(g: B => A): G[B] = fK(fa.imap(gK(ga))(f)(g))

object InvariantSyntax extends InvariantSyntax
