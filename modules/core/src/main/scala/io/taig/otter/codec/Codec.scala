package io.taig.otter.codec

trait Codec[S[_], T] extends Decoder[S, T], Encoder[S, T]
