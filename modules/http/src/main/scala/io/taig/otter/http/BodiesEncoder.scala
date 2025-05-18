package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.http.HttpError.*
import io.taig.otter.http.codec.PayloadEncoder
import io.taig.otter.http.header.MediaRange
import io.taig.otter.http.header.MediaType

final class BodiesEncoder[-S[_]](encoder: PayloadEncoder[S]):
  val writer = BodyEncoder(encoder)

  def apply[A](
      bodies: Bodies[S, A],
      accept: List[MediaRange],
      a: A
  ): Either[ContentNegotiationFailed, (MediaType, Array[Byte])] =
    bodies match
      case Bodies.Modify(self, _, g) => apply(bodies = self, accept, g(a))
      case Bodies.Or(left, right) =>
        accept
          .collectFirst:
            case mediaRange if left.satisfies(mediaRange)  => left
            case mediaRange if right.satisfies(mediaRange) => right
          .orElse(Option.when(accept.isEmpty)(left))
          .toRight(ContentNegotiationFailed)
          .flatMap(apply(_, accept, a))
      case Bodies.OrElse(left, right) =>
        a.fold(apply(bodies = left, accept, _), apply(bodies = right, accept, _))
      case Bodies.Root(body) => writer(body, accept, a).tupleLeft(body.mediaType)

  def apply[A](bodies: Bodies[S, A], contentType: Option[MediaType], a: A): Either[MediaTypeUnsupported, Array[Byte]] =
    bodies match
      case Bodies.Modify(self, _, g) => apply(bodies = self, contentType, g(a))
      case Bodies.Or(left, right) =>
        contentType
          .collectFirst:
            case mediaType if left.matches(mediaType)  => left
            case mediaType if right.matches(mediaType) => right
          .orElse(Option.when(contentType.isEmpty)(left))
          .toRight(MediaTypeUnsupported)
          .flatMap(apply(_, contentType, a))
      case Bodies.OrElse(left, right) =>
        a.fold(apply(bodies = left, contentType, _), apply(bodies = right, contentType, _))
      case Bodies.Root(body) => writer(body, contentType, a)

  def apply[A](bodies: Bodies[S, A], a: A): (MediaType, Array[Byte]) = bodies match
    case Bodies.Modify(self, _, g) => apply(bodies = self, g(a))
    case Bodies.Or(left, _)        => apply(bodies = left, a)
    case Bodies.OrElse(left, right) =>
      a.fold(apply(bodies = left, _), apply(bodies = right, _))
    case Bodies.Root(body) => (body.mediaType, writer(body, a))
