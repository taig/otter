package io.taig.otter.syntax

import cats.Contravariant

trait ContravariantSyntax:
  extension [F[_]](fa: Contravariant[F])
    def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): Contravariant[G] = new Contravariant[G]:
      override def contramap[A, B](ga: G[A])(f: B => A): G[B] = fK(fa.contramap(gK(ga))(f))

object ContravariantSyntax extends ContravariantSyntax
