package io.taig.otter.http.syntax

import io.taig.otter.http.header.MediaType
import io.taig.otter.http.Body
import io.taig.otter.Enrichment
import io.taig.otter.Reference

trait BodySyntax:
  def body[S[_], A](mediaType: MediaType, schema: => S[A]): Body[S, A] =
    Body(Enrichment(Body.Value.Root(mediaType, schema = Reference.later(schema))))

object BodySyntax extends BodySyntax
