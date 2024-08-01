package io.taig.otter.openapi

import io.taig.otter as Base
import io.taig.otter.Metadata

trait Keys extends Base.Keys:
  val description: Metadata.Key[String] = key("description")

  val format: Metadata.Key[String] = key("format")

object Keys extends Keys
