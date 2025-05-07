package io.taig.otter.http

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.http.header.MediaType
import cats.syntax.all.*

final class BodyDecoder[S[_]](decoder: PayloadDecoder[S]):
  def apply[A](
      codec: Body[S, A],
      contentType: Option[MediaType],
      bytes: Array[Byte]
  ): Validated[Violations, Option[A]] = codec match
    case Body.Modify(self, f, _) => apply(codec = self, contentType, bytes).map(_.map(f))
    case Body.Root(mediaType, codec) =>
      contentType match
        case Some(contentType) if contentType === mediaType =>
          decoder(codec = codec.value, contentType, bytes).map(_.some)
        case _ => none.valid
