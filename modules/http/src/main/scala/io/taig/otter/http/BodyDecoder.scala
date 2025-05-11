package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violations
import io.taig.otter.http.header.MediaType
import io.taig.otter.http.HttpError.MediaTypeUnsupported
import io.taig.otter.http.HttpError.ValidationViolations

final class BodyDecoder[S[_]](decoder: PayloadDecoder[S]):
  def apply[A](
      codec: Body[S, A],
      contentType: Option[MediaType],
      bytes: Array[Byte]
  ): Either[MediaTypeUnsupported | ValidationViolations, A] = codec match
    case Body.Modify(self, f, _) => apply(codec = self, contentType, bytes).map(f)
    case Body.Root(mediaType, codec) =>
      contentType match
        case Some(contentType) if contentType === mediaType =>
          decoder(codec = codec.value, contentType, bytes).toEither.leftMap(ValidationViolations.apply)
        case _ => MediaTypeUnsupported.asLeft
