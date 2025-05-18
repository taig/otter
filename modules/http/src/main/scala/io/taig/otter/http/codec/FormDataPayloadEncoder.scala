package io.taig.otter.http.codec

import cats.syntax.all.*
import io.taig.otter.*
import io.taig.otter.http.FormData

import java.nio.charset.StandardCharsets

object FormDataPayloadEncoder extends PayloadEncoder[FormData]:
  override def encode[A](schema: FormData[A], a: A): Array[Byte] =
    FormDataEncoder
      .encode(schema = schema, a)
      .map:
        case (key, None)        => key
        case (key, Some(value)) => s"$key=$value"
      .mkString_("&")
      .getBytes(StandardCharsets.UTF_8)
