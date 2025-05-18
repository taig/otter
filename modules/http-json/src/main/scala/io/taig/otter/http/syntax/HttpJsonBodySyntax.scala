package io.taig.otter.http.syntax

import io.taig.otter.http.Body
import io.taig.otter.http.syntax.BodySyntax.*
import io.taig.otter.http.syntax.MediaTypeSyntax.*
import io.taig.otter.Json

trait HttpJsonBodySyntax:
  def json[A](codec: => Json[A]): Body[Json, A] =
    body(mediaType = mediaType.application.json, codec)

object HttpJsonBodySyntax extends HttpJsonBodySyntax
