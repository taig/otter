package io.taig.otter.http

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.http.header.MediaType

final class Http4sBodiesDecoder[S[_]](decoder: PayloadDecoder[S]):
  def apply[A](
      bodies: Bodies[S, A],
      contentType: Option[MediaType],
      bytes: Array[Byte]
  ): Validated[Violations, Option[A]] =
    new BodiesDecoder(decoder)(codec = bodies, contentType, bytes)
