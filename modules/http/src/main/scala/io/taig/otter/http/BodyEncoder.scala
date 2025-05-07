package io.taig.otter.http

import io.taig.otter.http.Body.Modify
import io.taig.otter.http.Body.Root
import io.taig.otter.http.header.MediaRange

final class BodyEncoder[-S[_]](encoder: PayloadEncoder[S]):
  def apply[A](body: Body[S, A], accept: List[MediaRange], a: A): Option[Array[Byte]] = body match
    case Modify(self, f, g) => apply(body = self, accept, g(a))
    case Root(contentType, codec) =>
      Option.when(accept.exists(contentType.satisfies))(encoder(codec = codec.value, a))
