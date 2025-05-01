package io.taig.otter.http

import io.taig.otter.Json
import io.taig.otter.http.header.MediaType
import io.taig.otter.CirceJsonEncoder
import io.circe.Printer

final class CirceJsonBodyEncoder(printer: Printer) extends BodyEncoder[Json]:
  override def apply[A](mediaType: MediaType, codec: Json[A], a: A): Array[Byte] =
    // TODO extract chartset from media type (?)
    printer.print(CirceJsonEncoder(codec, a)).getBytes()

object CirceJsonBodyEncoder:
  def apply(printer: Printer): BodyEncoder[Json] = new CirceJsonBodyEncoder(printer)