package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Json
import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.CirceJsonDecoder
import io.circe.jawn.JawnParser
import io.taig.otter.Violation
import io.taig.otter.http.header.MediaType

final class CirceJsonPayloadDecoder extends PayloadDecoder[Json]:
  val parser = new JawnParser()

  // TODO support for alternative charsets
  override def apply[A](contentType: MediaType, codec: Json[A], bytes: Array[Byte]): Validated[Violations, A] = parser
    .parseByteArray(bytes)
    .toValidated
    .leftMap: failure =>
      Violations.rootNec(Violation.tpe(name = "json", actual = "unknown", hint = failure.show))
    .andThen(CirceJsonDecoder(codec, _))

object CirceJsonPayloadDecoder:
  val Default: PayloadDecoder[Json] = new CirceJsonPayloadDecoder()
