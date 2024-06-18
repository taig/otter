package io.taig.otter.json.circe

import io.taig.otter.schema.SchemaTest
import io.taig.otter.Decoder
import io.taig.otter.Encoder
import io.circe.Json
import io.taig.otter.Plain

final class JsonCirceSchemaTest extends SchemaTest[Json], Plain:
  override val decoder: Decoder[Schema.Reader, Json] = JsonDecoder
  override val encoder: Encoder[Schema.Writer, Json] = JsonEncoder
