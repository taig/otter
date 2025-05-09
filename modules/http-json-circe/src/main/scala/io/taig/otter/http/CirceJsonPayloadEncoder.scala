package io.taig.otter.http

import io.circe.Printer
import io.taig.otter.CirceJsonEncoder
import io.taig.otter.Json

import java.nio.charset.StandardCharsets

final class CirceJsonPayloadEncoder(printer: Printer) extends PayloadEncoder[Json]:
  override def apply[A](codec: Json[A], a: A): Array[Byte] =
    printer.print(CirceJsonEncoder(codec, a)).getBytes(StandardCharsets.UTF_8)

object CirceJsonPayloadEncoder:
  def apply(printer: Printer): PayloadEncoder[Json] = new CirceJsonPayloadEncoder(printer)
