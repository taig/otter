package io.taig.otter.http.codec

import cats.syntax.all.*
import io.taig.otter.http.Body
import io.taig.otter.http.HttpError.*
import io.taig.otter.http.header.MediaRange
import io.taig.otter.http.header.MediaType

final class BodyEncoder[-S[_]](encoder: PayloadEncoder[S]):
  def encode[A](
      schema: Body[S, A],
      accept: List[MediaRange],
      a: A
  ): Either[ContentNegotiationFailed, Array[Byte]] =
    if (accept.isEmpty) then encode(schema = schema.value, a).asRight
    else
      Either.cond(
        test = accept.exists(schema.mediaType.satisfies),
        right = encode(schema = schema.value, a),
        left = {
          println("encode body")
          ContentNegotiationFailed
        }
      )

  def encode[A](
      schema: Body[S, A],
      contentType: Option[MediaType],
      a: A
  ): Either[MediaTypeUnsupported, Array[Byte]] = encode(schema = schema.value, contentType, a)

  def encode[A](
      schema: Body.Value[S, A],
      contentType: Option[MediaType],
      a: A
  ): Either[MediaTypeUnsupported, Array[Byte]] = contentType match
    case Some(contentType) =>
      Either.cond(
        test = schema.mediaType === contentType,
        right = encode(schema, a),
        left = MediaTypeUnsupported
      )
    case None => encode(schema, a).asRight

  def encode[A](schema: Body[S, A], a: A): Array[Byte] = encode(schema = schema.value, a)

  def encode[A](schema: Body.Value[S, A], a: A): Array[Byte] = schema match
    case Body.Value.Modify(self, f, g)        => encode(schema = self, g(a))
    case Body.Value.Root(contentType, schema) => encoder.encode(schema = schema.value, a)
