package io.taig.otter.codec

trait Codec[S[_], T] extends Decoder[S, T], Encoder[S, T]:
  inline def asDecoder: Decoder[S, T] = this

  inline def asEncoder: Encoder[S, T] = this

  final override def mapK[U[_]](fK: [A] => U[A] => S[A]): Codec[U, T] =
    Codec(decoder = asDecoder.mapK(fK), encoder = asEncoder.mapK(fK))

object Codec:
  def apply[S[_], T](decoder: Decoder[S, T], encoder: Encoder[S, T]): Codec[S, T] =
    new Codec[S, T]:
      export decoder.*
      export encoder.*
