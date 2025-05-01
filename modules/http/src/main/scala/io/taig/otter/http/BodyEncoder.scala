package io.taig.otter.http

import io.taig.otter.http.header.MediaType

abstract class BodyEncoder[S[_]]:
  def apply[A](mediaType: MediaType, codec: S[A], a: A): Array[Byte]
