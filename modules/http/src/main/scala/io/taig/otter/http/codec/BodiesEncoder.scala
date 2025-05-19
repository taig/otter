package io.taig.otter.http.codec

import cats.syntax.all.*
import io.taig.otter.http.Bodies
import io.taig.otter.http.HttpError.*
import io.taig.otter.http.header.MediaRange
import io.taig.otter.http.header.MediaType

final class BodiesEncoder[-S[_]](encoder: PayloadEncoder[S]):
  val body = BodyEncoder(encoder)

  def encode[A](
      schema: Bodies[S, A],
      accept: List[MediaRange],
      a: A
  ): Either[ContentNegotiationFailed, (MediaType, Array[Byte])] =
    schema match
      case Bodies.Modify(self, _, g) => encode(schema = self, accept, g(a))
      case Bodies.Or(left, right) =>
        accept
          .collectFirst:
            case mediaRange if left.satisfies(mediaRange)  => left
            case mediaRange if right.satisfies(mediaRange) => right
          .orElse(Option.when(accept.isEmpty)(left))
          .toRight(ContentNegotiationFailed)
          .flatMap(encode(_, accept, a))
      case Bodies.OrElse(left, right) =>
        a.fold(encode(schema = left, accept, _), encode(schema = right, accept, _))
      case Bodies.Root(body) => this.body.encode(body, accept, a).tupleLeft(body.mediaType)

  def encode[A](schema: Bodies[S, A], contentType: Option[MediaType], a: A): Either[MediaTypeUnsupported, Array[Byte]] =
    schema match
      case Bodies.Modify(self, _, g) => encode(schema = self, contentType, g(a))
      case Bodies.Or(left, right) =>
        contentType
          .collectFirst:
            case mediaType if left.matches(mediaType)  => left
            case mediaType if right.matches(mediaType) => right
          .orElse(Option.when(contentType.isEmpty)(left))
          .toRight(MediaTypeUnsupported)
          .flatMap(encode(_, contentType, a))
      case Bodies.OrElse(left, right) =>
        a.fold(encode(schema = left, contentType, _), encode(schema = right, contentType, _))
      case Bodies.Root(body) => this.body.encode(body, contentType, a)

  def encode[A](schema: Bodies[S, A], a: A): (MediaType, Array[Byte]) = schema match
    case Bodies.Modify(self, _, g) => encode(schema = self, g(a))
    case Bodies.Or(left, _)        => encode(schema = left, a)
    case Bodies.OrElse(left, right) =>
      a.fold(encode(schema = left, _), encode(schema = right, _))
    case Bodies.Root(body) => (body.mediaType, this.body.encode(body, a))
