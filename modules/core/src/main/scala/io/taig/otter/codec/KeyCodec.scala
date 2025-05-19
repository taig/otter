package io.taig.otter.codec

import io.taig.otter.Key

object KeyCodec:
  val Quoted: Codec[Key, String] = Codec(decoder = KeyParser.Quoted, encoder = KeyPrinter.Quoted)
  val Unquoted: Codec[Key, String] = Codec(decoder = KeyParser.Unquoted, encoder = KeyPrinter.Unquoted)
