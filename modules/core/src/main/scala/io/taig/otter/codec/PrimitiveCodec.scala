package io.taig.otter.codec

import io.taig.otter.Primitive

object PrimitiveCodec:
  val Quoted: Codec[Primitive, String] = Codec(decoder = PrimitiveParser.Quoted, encoder = PrimitivePrinter.Quoted)
  val Unquoted: Codec[Primitive, String] =
    Codec(decoder = PrimitiveParser.Unquoted, encoder = PrimitivePrinter.Unquoted)
