package io.taig.otter

trait Translator[F[_], G[_]]:
  def translate[A](fa: F[A]): G[A]

object Translator:
  inline def apply[F[_], G[_]](using translator: Translator[F, G]): Translator[F, G] = translator