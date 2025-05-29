package io.taig.otter.http.syntax

import io.taig.otter.Json
import io.taig.otter.http.Body
import io.taig.otter.http.syntax.MediaTypeSyntax.*

trait HttpJsonBodySyntax:
  def body[A](schema: => Json[A]): Body[Json, A] =
    BodySyntax.body(mediaType = mediaType.application.json, schema)

object HttpJsonBodySyntax extends HttpJsonBodySyntax
