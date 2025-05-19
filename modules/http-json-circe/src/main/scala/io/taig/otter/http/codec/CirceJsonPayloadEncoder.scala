package io.taig.otter.http.codec

import io.circe.Printer
import io.taig.otter.Json
import io.taig.otter.codec.CirceJsonEncoder

import java.nio.charset.StandardCharsets

final class CirceJsonPayloadEncoder(printer: Printer) extends PayloadEncoder[Json]:
  override def encode[A](codec: Json[A], a: A): Array[Byte] =
    printer.print(CirceJsonEncoder.encode(codec, a)).getBytes(StandardCharsets.UTF_8)

object CirceJsonPayloadEncoder:
  def apply(printer: Printer): PayloadEncoder[Json] = new CirceJsonPayloadEncoder(printer)
