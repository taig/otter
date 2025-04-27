package io.taig.otter

import io.taig.otter.http.Body
import io.taig.otter.http.BodyDsl
import io.taig.otter.http.header.MediaType
import io.taig.otter.http.header.Parameters

trait HttpJsonDsl:
  def json[A](codec: => Json[A]): Body[Json, A] = BodyDsl.body(
    mediaType = MediaType(
      tpe = MediaType.Type(primary = "application", secondary = "json"),
      parameters = Parameters.Empty
    ),
    codec
  )

object HttpJsonDsl extends HttpJsonDsl
