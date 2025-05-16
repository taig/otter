package io.taig.otter.codec

trait Codec[S[_], T] extends Decoder[S, T], Encoder[S, T]

object Codec:
  def apply[S[_], T](decoder: Decoder[S, T], encoder: Encoder[S, T]): Codec[S, T] =
    new Codec[S, T]:
      export decoder.*
      export encoder.*
