package io.taig.otter

trait Translator[F[_], G[_]]:
  def translate[A](fa: F[A]): G[A]
