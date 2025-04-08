package io.taig.otter.http

import io.taig.otter.Metadata

trait HttpKeys:
  val explode: Metadata.Key[Boolean] = Metadata.Key("explode")
  val serialization: Metadata.Key[Serialization] = Metadata.Key("serialization")

object HttpKeys extends HttpKeys
