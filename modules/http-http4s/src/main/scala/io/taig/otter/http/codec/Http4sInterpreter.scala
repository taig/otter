package io.taig.otter.http.codec

import fs2.Stream
import io.taig.otter.http.Body

/** Independently registered whole-document and stream capabilities. */
object Http4sInterpreter:
  type Supported[F[_], P[-w, +r], Q[-w, +r]] =
    Body.Or[Http4sPayload.Supported[P], Body.Streamed.Requirement[Stream[F, +*], Q]]

  trait Of[P[-_, +_], Q[-_, +_]]:
    def buffered: Http4sPayload[P]
    def streams: Http4sStreams[Q]
