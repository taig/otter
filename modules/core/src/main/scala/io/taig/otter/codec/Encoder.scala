package io.taig.otter.codec

trait Encoder[-F[_], T]:
  self =>

  def encode[A](fa: F[A], a: A): T

  def contramapK[G[_]](fK: [A] => G[A] => F[A]): Encoder[G, T] = new Encoder[G, T]:
    override def encode[A](ga: G[A], a: A): T = self.encode(fK(ga), a)
