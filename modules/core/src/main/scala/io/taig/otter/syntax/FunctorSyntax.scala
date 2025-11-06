package io.taig.otter.syntax

import cats.Functor

trait FunctorSyntax:
  extension [F[_]](fa: Functor[F])
    def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): Functor[G] = new Functor[G]:
      override def map[A, B](ga: G[A])(f: A => B): G[B] = fK(fa.map(gK(ga))(f))
