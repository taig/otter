package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.http.header.MediaRange
import io.taig.otter.http.header.MediaType

final class BodiesEncoder[-S[_]](encoder: PayloadEncoder[S]):
  val write = BodyEncoder(encoder)

  def apply[A](bodies: Bodies[S, A], accept: List[MediaRange], a: A): Option[Array[Byte]] = bodies match
    case Bodies.Modify(self, _, g) => apply(bodies = self, accept, g(a))
    case Bodies.Or(left, right) =>
      accept
        .collectFirstSome: mediaRange =>
          if left.satisfies(mediaRange) then apply(bodies = left, accept, a)
          else if right.satisfies(mediaRange) then apply(bodies = right, accept, a)
          else none
        .orElse(Option.when(accept.isEmpty)(apply(bodies = left, accept, a)).flatten)
    case Bodies.OrElse(left, right) =>
      a.fold(apply(bodies = left, accept, _), apply(bodies = right, accept, _))
    case Bodies.Root(body) => write(body, accept, a)

  def apply[A](bodies: Bodies[S, A], contentType: Option[MediaType], a: A): Option[Array[Byte]] = bodies match
    case Bodies.Modify(self, _, g) => apply(bodies = self, contentType, g(a))
    case Bodies.Or(left, right) =>
      contentType
        .flatMap: mediaType =>
          if left.matches(mediaType) then apply(bodies = left, contentType, a)
          else if right.matches(mediaType) then apply(bodies = right, contentType, a)
          else none
        .orElse(Option.when(contentType.isEmpty)(apply(bodies = left, contentType, a)).flatten)
    case Bodies.OrElse(left, right) =>
      a.fold(apply(bodies = left, contentType, _), apply(bodies = right, contentType, _))
    case Bodies.Root(body) => write(body, contentType, a)
