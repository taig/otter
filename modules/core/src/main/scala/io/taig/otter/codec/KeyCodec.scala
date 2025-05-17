package io.taig.otter.codec

import io.taig.otter.Key

val KeyCodec: Codec[Key, String] = Codec(decoder = KeyParser, encoder = KeyPrinter)
