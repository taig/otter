package io.taig.otter

import cats.arrow.*
import cats.~>

trait Translator[F[_], G[_]] extends (F ~> G):
  override final def apply[A](fa: F[A]): G[A] = translate(fa)

  def translate[A](fa: F[A]): G[A]