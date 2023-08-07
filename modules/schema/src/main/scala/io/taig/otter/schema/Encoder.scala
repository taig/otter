package io.taig.otter.schema

trait Encoder[F[_], +A]:
  self =>

  def encode[B](fb: F[B], b: B): A

  final def map[B](f: A => B): Encoder[F, B] = new Encoder[F, B]:
    override def encode[C](fc: F[C], c: C): B = f(self.encode(fc, c))
