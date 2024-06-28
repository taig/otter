package io.taig.otter

import cats.Applicative
import cats.Comonad

trait ApplicativeComonad[F[_]] extends Applicative[F], Comonad[F]

object ApplicativeComonad:
  inline def apply[F[_]](using F: ApplicativeComonad[F]): ApplicativeComonad[F] = F
