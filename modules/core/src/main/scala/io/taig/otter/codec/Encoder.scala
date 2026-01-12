package io.taig.otter.codec

trait Encoder[F[_], A]:
  self =>

  def encode[B](fb: F[B], b: B): A

  def contramapK[G[_]](fK: [A] => G[A] => F[A]): Encoder[G, A] = new Encoder[G, A]:
    override def encode[B](gb: G[B], b: B): A = self.encode(fK(gb), b)
