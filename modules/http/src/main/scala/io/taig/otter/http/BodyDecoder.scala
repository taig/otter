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
    // case Body.Empty              => ().some.valid
    case Body.Modify(self, f, _) => apply(codec = self, contentType, bytes).map(_.map(f))
    // case Body.Or(left, right) =>
    //   apply(contentType, codec = left, bytes).andThen:
    //     case a @ Some(_) => a.valid
    //     case None        => apply(contentType, codec = right, bytes)
    // case Body.OrElse(left, right) =>
    //   apply(contentType, codec = left, bytes)
    //     .map(_.map(Left(_)))
    //     .orElse(apply(contentType, codec = right, bytes).map(_.map(Right(_))))
    case Body.Root(mediaType, codec) =>
      contentType match
        case Some(contentType) if contentType === mediaType =>
          decoder(contentType, codec = codec.value, bytes).map(_.some)
        case _ => none.valid
