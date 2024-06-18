package io.taig.otter

import cats.Apply
import cats.Applicative
import cats.Comonad

trait ApplicativeComonad[F[_]] extends Applicative[F], Comonad[F]
