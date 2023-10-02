package io.taig.otter.openapi

import cats.data.Chain

opaque type SecurityRequirement = Chain[(String, Chain[String])]

object SecurityRequirement:
  extension (self: SecurityRequirement) def toChain: Chain[(String, Chain[String])] = self
