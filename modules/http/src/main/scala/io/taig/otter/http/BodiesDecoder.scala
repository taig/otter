package io.taig.otter.http

import io.taig.otter.http.header.MediaType
import cats.data.Validated
import io.taig.otter.Violations

final class BodiesDecoder[S[_]](decoder: PayloadDecoder[S]):
  def apply[A](
      codec: Bodies[S, A],
      contentType: Option[MediaType],
      bytes: Array[Byte]
  ): Validated[Violations, Option[A]] = ???
  