package io.taig.otter.http

import io.taig.otter.http.header.MediaType

final class FormDataBodyEncoder extends BodyEncoder[FormData]:
  override def apply[A](mediaType: MediaType, codec: FormData[A], a: A): Array[Byte] = ???

object FormDataBodyEncoder:
  val Default: BodyEncoder[FormData] = new FormDataBodyEncoder()
