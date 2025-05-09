package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violations
import io.taig.otter.http.header.MediaType
import org.http4s.Header as Http4sHeader
import org.typelevel.ci.*
import io.taig.otter.Violation

final class Http4sBodiesDecoder[S[_]](decoder: PayloadDecoder[S]):
  def apply[A](
      bodies: Bodies[S, A],
      contentType: Option[MediaType],
      bytes: Array[Byte]
  ): Validated[Violations, Option[A]] =
    new BodiesDecoder(decoder)(codec = bodies, contentType, bytes)
