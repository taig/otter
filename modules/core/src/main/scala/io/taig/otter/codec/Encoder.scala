package io.taig.otter.codec

trait Encoder[S[_], T]:
  self =>

  def apply[A](schema: S[A], a: A): T

  final def map[B](f: T => B): Encoder[S, B] = new Encoder[S, B]:
    override def apply[A](codec: S[A], a: A): B = f(self(codec, a))
