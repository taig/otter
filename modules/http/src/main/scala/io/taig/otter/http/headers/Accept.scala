package io.taig.otter.http.headers

import io.taig.otter.http.MediaType

final case class Accept(mediaType: MediaType, weight: Option[Accept.Weight])

object Accept:
  opaque type Weight = Float
