package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.http.HttpError.*
import io.taig.otter.http.header.MediaRange
import io.taig.otter.http.header.MediaType

final class BodyEncoder[-S[_]](encoder: PayloadEncoder[S]):
  def apply[A](
      body: Body[S, A],
      accept: List[MediaRange],
      a: A
  ): Either[ContentNegotiationFailed, Array[Byte]] =
    if (accept.isEmpty) then apply(body, a).asRight
    else
      Either.cond(
        test = accept.exists(body.mediaType.satisfies),
        right = apply(body, a),
        left = ContentNegotiationFailed
      )

  def apply[A](body: Body[S, A], contentType: Option[MediaType], a: A): Either[MediaTypeUnsupported, Array[Byte]] =
    contentType match
      case Some(contentType) =>
        Either.cond(
          test = body.mediaType === contentType,
          right = apply(body, a),
          left = MediaTypeUnsupported
        )
      case None => apply(body, a).asRight

  def apply[A](body: Body[S, A], a: A): Array[Byte] = body match
    case Body.Modify(self, f, g)       => apply(body = self, g(a))
    case Body.Root(contentType, codec) => encoder(codec = codec.value, a)
