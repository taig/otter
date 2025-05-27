package io.taig.otter.http.syntax

import io.taig.otter.http.Body
import io.taig.otter.http.FormData
import io.taig.otter.http.syntax.BodySyntax.*
import io.taig.otter.http.syntax.MediaTypeSyntax.*

trait FormDataBodySyntax:
  def formData[A](schema: => FormData[A]): Body[FormData, A] =
    body(mediaType = mediaType.application.wwwFormUrlencoded, schema)
