package io.taig.otter.sample.tapir

import sttp.tapir.*

object SampleTapirApp {
  val x: Endpoint[Unit, String, Unit, Unit, Any] = endpoint.in(stringBody)
}
