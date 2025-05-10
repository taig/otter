package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.http.header.MediaType
import io.taig.otter.http.header.MediaRange

final class BodyEncoder[-S[_]](encoder: PayloadEncoder[S]):
  def apply[A](body: Body[S, A], accept: List[MediaRange], a: A): Option[Array[Byte]] = body match
    case Body.Modify(self, f, g) => apply(body = self, accept, g(a))
    case Body.Root(contentType, codec) =>
      if (accept.isEmpty) then encoder(codec = codec.value, a).some
      else Option.when(accept.exists(contentType.satisfies))(encoder(codec = codec.value, a))

  def apply[A](body: Body[S, A], contentType: Option[MediaType], a: A): Option[Array[Byte]] =
    body match
      case Body.Modify(self, f, g) => apply(body = self, contentType, g(a))
      case Body.Root(mediaType, codec) =>
        contentType.fold(encoder(codec = codec.value, a).some): contentType =>
          Option.when(mediaType === contentType)(encoder(codec = codec.value, a))
