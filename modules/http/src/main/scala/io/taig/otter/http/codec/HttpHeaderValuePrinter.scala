package io.taig.otter.http.codec

import io.taig.otter.codec.Encoder
import io.taig.otter.http.Http
import io.taig.otter.http.Http.Header.Value
import io.taig.otter.codec.KeyPrinter

object HttpHeaderValuePrinter extends Encoder[Http.Header.Value, String]:
  override def encode[A](schema: Value[A], a: A): String = KeyPrinter.encode(schema = schema.self, a)
