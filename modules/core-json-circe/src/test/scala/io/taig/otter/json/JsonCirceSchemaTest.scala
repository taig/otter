package io.taig.otter.json

import io.taig.otter.CodecTest
import io.taig.otter.Decoder
import io.taig.otter.Encoder
import io.circe.Json
import io.taig.otter.Plain

final class JsonCirceSchemaTest extends CodecTest[Json], Plain:
  override val decoder: Decoder[Schema.Reader, Json] = JsonDecoder
  override val encoder: Encoder[Schema.Writer, Json] = JsonEncoder
