package io.taig.otter.http

import io.taig.otter.http.HttpError.*
import io.taig.otter.http.codec.PayloadDecoder
import io.taig.otter.http.header.MediaType

final class BodiesDecoder[-S[_]](decoder: PayloadDecoder[S]):
  def apply[A](
      schema: Bodies[S, A],
      contentType: Option[MediaType],
      bytes: Array[Byte]
  ): Either[MediaTypeUnsupported | ValidationViolations, A] = schema match
    case Bodies.Modify(self, f, _) => apply(schema = self, contentType, bytes).map(f)
    case Bodies.Or(left, right) =>
      apply(schema = left, contentType, bytes) match
        case result @ Right(_)                      => result
        case Left(MediaTypeUnsupported)             => apply(schema = right, contentType, bytes)
        case result @ Left(ValidationViolations(_)) => result
    case Bodies.OrElse(left, right) =>
      apply(schema = left, contentType, bytes)
        .map(Left(_))
        .orElse(apply(schema = right, contentType, bytes).map(Right(_)))
    case Bodies.Root(body) => BodyDecoder(decoder)(schema = body, contentType, bytes)
