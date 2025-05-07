package io.taig.otter.http

import io.taig.otter.http.header.MediaType

final class FormDataPayloadEncoder extends PayloadEncoder[FormData]:
  override def apply[A](codec: FormData[A], a: A): Array[Byte] = ???

object FormDataPayloadEncoder:
  val Default: PayloadEncoder[FormData] = new FormDataPayloadEncoder()
