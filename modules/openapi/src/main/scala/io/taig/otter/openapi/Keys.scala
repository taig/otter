package io.taig.otter.openapi

import io.taig.otter as Base
import io.taig.otter.Metadata

trait Keys extends Base.Keys:
  val description: Metadata.Key[String] = key("description")
  val format: Metadata.Key[String] = key("format")
  val operationId: Metadata.Key[String] = key("operationId")
  val summary: Metadata.Key[String] = key("summary")
  val tags: Metadata.Key[Vector[String]] = key("tags")

object Keys extends Keys
