package io.taig.otter

import scala.Product as SProduct

opaque type Metadata = Map[String, Any]

object Metadata:
  opaque type Key[A] = String
