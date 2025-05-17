package io.taig.otter.codec

trait Encoder[S[_], T]:
  self =>

  def encode[A](schema: S[A], a: A): T

  final def map[B](f: T => B): Encoder[S, B] = new Encoder[S, B]:
    override def encode[A](codec: S[A], a: A): B = f(self.encode(codec, a))

  def mapK[U[_]](fK: [A] => U[A] => S[A]): Encoder[U, T] = new Encoder[U, T]:
    override def encode[A](schema: U[A], a: A): T = self.encode(schema = fK(schema), a)

  final def toCodec(decoder: Decoder[S, T]): Codec[S, T] = Codec(decoder, encoder = this)
