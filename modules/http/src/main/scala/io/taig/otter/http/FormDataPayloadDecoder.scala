package io.taig.otter.http

import cats.data.Validated

import io.taig.otter.Violations
import io.taig.otter.http.header.MediaType

final class FormDataPayloadDecoder extends PayloadDecoder[FormData]:
  override def apply[A](contentType: MediaType, codec: FormData[A], bytes: Array[Byte]): Validated[Violations, A] =
    ???

object FormDataPayloadDecoder:
  val Default: PayloadDecoder[FormData] = new FormDataPayloadDecoder()
