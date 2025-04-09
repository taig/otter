package io.taig.otter.http

import io.taig.otter.Metadata

trait HttpKeys:
  val explode: Metadata.Key[Boolean] = Metadata.Key("explode")
  val style: Metadata.Key[Header.Style | Query.Style] = Metadata.Key("style")

object HttpKeys extends HttpKeys
