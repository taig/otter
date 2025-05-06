package io.taig.otter.http

import io.taig.otter.Json
import io.taig.otter.http.header.MediaType
import io.taig.otter.CirceJsonEncoder
import io.circe.Printer

final class CirceJsonPayloadEncoder(printer: Printer) extends PayloadEncoder[Json]:
  override def apply[A](contentType: MediaType, codec: Json[A], a: A): Array[Byte] =
    // TODO extract chartset from media type (?)
    printer.print(CirceJsonEncoder(codec, a)).getBytes()

object CirceJsonPayloadEncoder:
  def apply(printer: Printer): PayloadEncoder[Json] = new CirceJsonPayloadEncoder(printer)
