package io.taig.otter.http.codec

import io.taig.otter.http.Bodies
import io.taig.otter.http.HttpError.*
import io.taig.otter.http.header.MediaType

final class BodiesDecoder[-S[_]](decoder: PayloadDecoder[S]):
  val body = BodyDecoder(decoder)

  def decode[A](
      schema: Bodies[S, A],
      contentType: Option[MediaType],
      bytes: Array[Byte]
  ): Either[MediaTypeUnsupported | ValidationViolations, A] =
    decode(schema = schema.value, contentType, bytes)

  def decode[A](
      schema: Bodies.Value[S, A],
      contentType: Option[MediaType],
      bytes: Array[Byte]
  ): Either[MediaTypeUnsupported | ValidationViolations, A] = schema match
    case Bodies.Value.Modify(self, f, _) => decode(schema = self, contentType, bytes).map(f)
    case Bodies.Value.Or(left, right) =>
      decode(schema = left, contentType, bytes) match
        case result @ Right(_)                      => result
        case Left(MediaTypeUnsupported)             => decode(schema = right, contentType, bytes)
        case result @ Left(ValidationViolations(_)) => result
    case Bodies.Value.OrElse(left, right) =>
      decode(schema = left, contentType, bytes)
        .map(Left(_))
        .orElse(decode(schema = right, contentType, bytes).map(Right(_)))
    case Bodies.Value.Root(body) => this.body.decode(schema = body, contentType, bytes)
