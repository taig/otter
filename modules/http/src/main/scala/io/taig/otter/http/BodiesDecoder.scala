package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violations
import io.taig.otter.http.HttpError.*
import io.taig.otter.http.header.MediaType

final class BodiesDecoder[-S[_]](decoder: PayloadDecoder[S]):
  def apply[A](
      codec: Bodies[S, A],
      contentType: Option[MediaType],
      bytes: Array[Byte]
  ): Either[MediaTypeUnsupported | ValidationViolations, A] = codec match
    case Bodies.Modify(self, f, _) => apply(codec = self, contentType, bytes).map(f)
    case Bodies.Or(left, right) =>
      apply(codec = left, contentType, bytes) match
        case result @ Right(_)          => result
        case Left(MediaTypeUnsupported) => apply(codec = right, contentType, bytes)
        case result @ Left(ValidationViolations(_)) => result
    case Bodies.OrElse(left, right) =>
      apply(codec = left, contentType, bytes)
        .map(Left(_))
        .orElse(apply(codec = right, contentType, bytes).map(Right(_)))
    case Bodies.Root(body) => BodyDecoder(decoder)(codec = body, contentType, bytes)
