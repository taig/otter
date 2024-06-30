package io.taig.otter

import cats.Applicative
import cats.Comonad

type ApplicativeComonad[F[_]] = Applicative[F] & Comonad[F]

object ApplicativeComonad:
  inline def apply[F[_]](using F: Applicative[F] & Comonad[F]): ApplicativeComonad[F] = F
