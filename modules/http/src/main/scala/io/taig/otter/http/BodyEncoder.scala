package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.http.header.MediaRange
import io.taig.otter.http.header.MediaType
import io.taig.otter.http.HttpError.*

final class BodyEncoder[-S[_]](encoder: PayloadEncoder[S]):
  def apply[A](
      body: Body[S, A],
      accept: List[MediaRange],
      a: A
  ): Either[ContentNegotiationFailed, (MediaType, Array[Byte])] =
    body match
      case Body.Modify(self, f, g) => apply(body = self, accept, g(a))
      case Body.Root(contentType, codec) =>
        if (accept.isEmpty) then (contentType, encoder(codec = codec.value, a)).asRight
        else
          Either.cond(
            test = accept.exists(contentType.satisfies),
            right = (contentType, encoder(codec = codec.value, a)),
            left = ContentNegotiationFailed
          )

  def apply[A](body: Body[S, A], contentType: Option[MediaType], a: A): Either[MediaTypeUnsupported, Array[Byte]] =
    body match
      case Body.Modify(self, f, g) => apply(body = self, contentType, g(a))
      case Body.Root(mediaType, codec) =>
        contentType match
          case Some(contentType) =>
            Either.cond(
              test = mediaType === contentType,
              right = encoder(codec = codec.value, a),
              left = MediaTypeUnsupported
            )
          case None => encoder(codec = codec.value, a).asRight
