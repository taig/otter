package io.taig.otter.openapi

import cats.data.Chain

final case class Responses(values: Chain[(Int, Extended[Response])] = Chain.empty, default: Option[Response] = None)
