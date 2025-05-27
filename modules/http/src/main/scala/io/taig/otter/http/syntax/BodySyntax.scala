package io.taig.otter.http.syntax

import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.http.Body
import io.taig.otter.http.header.MediaType

trait BodySyntax:
  def body[S[_], A](mediaType: MediaType, schema: => S[A]): Body[S, A] = Body(
    value = Body.Value.Root(mediaType, schema = Reference.later(schema)),
    metadata = Metadata.Empty
  )

object BodySyntax extends BodySyntax
