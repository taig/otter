package io.taig.otter.http.syntax

import io.taig.otter.Reference
import io.taig.otter.http.header.MediaType
import io.taig.otter.http.Body

trait BodySyntax:
  def body[S[_], A](mediaType: MediaType, codec: => S[A]): Body[S, A] =
    Body.Root(mediaType, codec = Reference.later(codec))

object BodySyntax extends BodySyntax
