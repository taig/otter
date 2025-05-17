package io.taig.otter.codec

import io.taig.otter.Json

val JsonKeyCodec: Codec[Json.Key, String] = Codec(decoder = JsonKeyParser, encoder = JsonKeyPrinter)
