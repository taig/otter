package io.taig.otter.http

import io.taig.otter.http.header.MediaType
import cats.data.Validated
import io.taig.otter.Violations
import cats.syntax.all.*

final class BodiesDecoder[S[_]](decoder: PayloadDecoder[S]):
  def apply[A](
      codec: Bodies[S, A],
      contentType: Option[MediaType],
      bytes: Array[Byte]
  ): Validated[Violations, Option[A]] = codec match
    case Bodies.Modify(self, f, _) =>  apply(codec = self, contentType, bytes).map(_.map(f))
    case Bodies.Or(left, right) => 
      apply(codec = left, contentType, bytes).andThen:
        case a @ Some(_) => a.valid
        case None        => apply(codec = right, contentType, bytes)
    case Bodies.OrElse(left, right) => 
      apply(codec = left, contentType, bytes)
        .map(_.map(Left(_)))
        .orElse(apply(codec = right, contentType, bytes).map(_.map(Right(_))))
    case Bodies.Root(body) => BodyDecoder(decoder)(codec = body, contentType, bytes)
  
