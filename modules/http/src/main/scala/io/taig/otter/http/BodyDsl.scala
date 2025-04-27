package io.taig.otter.http

import io.taig.otter.Reference
import io.taig.otter.http.header.MediaType

trait BodyDsl:
  object body:
    val empty: Body[Nothing, Unit] = Body.Empty

    def apply[S[_], A](mediaType: MediaType, codec: => S[A]): Body[S, A] =
      Body.Root(mediaType, codec = Reference.later(codec))

object BodyDsl extends BodyDsl
