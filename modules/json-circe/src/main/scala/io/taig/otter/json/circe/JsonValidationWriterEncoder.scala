package io.taig.otter.json.circe

import io.taig.otter.ValidationWriter
import io.circe.Json

object ValidationWriterJsonEncoder:
  def apply[A](writer: ValidationWriter[A]): Json = writer match
    case ValidationWriter.Root(writer, value) => JsonEncoder(writer, value)
