package io.taig.otter.http

import io.taig.otter.+
import io.taig.otter.http.header.MediaType

final class BodyEncoder[-S[_]](encoder: PayloadEncoder[S]):
  def apply[A](mediaType: MediaType, codec: S[A], a: A): Array[Byte] = ???
