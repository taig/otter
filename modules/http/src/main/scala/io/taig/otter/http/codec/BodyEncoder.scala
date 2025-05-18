package io.taig.otter.http.codec

import cats.syntax.all.*
import io.taig.otter.http.HttpError.*
import io.taig.otter.http.header.MediaRange
import io.taig.otter.http.header.MediaType
import io.taig.otter.http.Body

final class BodyEncoder[-S[_]](encoder: PayloadEncoder[S]):
  def encode[A](
      schema: Body[S, A],
      accept: List[MediaRange],
      a: A
  ): Either[ContentNegotiationFailed, Array[Byte]] =
    if (accept.isEmpty) then encode(schema, a).asRight
    else
      Either.cond(
        test = accept.exists(schema.mediaType.satisfies),
        right = encode(schema, a),
        left = ContentNegotiationFailed
      )

  def encode[A](schema: Body[S, A], contentType: Option[MediaType], a: A): Either[MediaTypeUnsupported, Array[Byte]] =
    contentType match
      case Some(contentType) =>
        Either.cond(
          test = schema.mediaType === contentType,
          right = encode(schema, a),
          left = MediaTypeUnsupported
        )
      case None => encode(schema, a).asRight

  def encode[A](schema: Body[S, A], a: A): Array[Byte] = schema match
    case Body.Modify(self, f, g)        => encode(schema = self, g(a))
    case Body.Root(contentType, schema) => encoder.encode(schema = schema.value, a)
