package io.taig.otter.http

import io.taig.otter.Reference
import io.taig.otter.http.header.MediaType

trait BodyDsl:
  def body[S[_], A](mediaType: MediaType, codec: => S[A]): Body[S, A] =
      Body.Root(mediaType, codec = Reference.later(codec))

object BodyDsl extends BodyDsl
