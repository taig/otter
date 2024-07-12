package io.taig.otter

opaque type Metadata = Map[String, Any]

object Metadata:
  opaque type Key[A] = String

  val Empty: Metadata = Map.empty
