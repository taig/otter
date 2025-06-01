package io.taig.otter.codec

import io.taig.otter.Primitive

object PrimitiveCodec:
  def apply[S[_]](codec: Codec[S, String])(quotes: Boolean): Codec[Primitive[S, *], String] =
    Codec(decoder = PrimitiveParser(parser = codec)(quotes), encoder = PrimitivePrinter(printer = codec)(quotes))
