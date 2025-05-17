package io.taig.otter.http.codec

import io.taig.otter.codec.Encoder
import io.taig.otter.http.Http
import io.taig.otter.http.Http.Header.Value
import io.taig.otter.codec.KeyPrinter
import io.taig.otter.codec.Decoder
import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.KeyParser

object HttpHeaderValueParser extends Decoder[Http.Header.Value, String]:
  override def decode[A](schema: Value[A], value: String): Validated[Violations, A] =
    KeyParser.decode(schema = schema.self, value)
