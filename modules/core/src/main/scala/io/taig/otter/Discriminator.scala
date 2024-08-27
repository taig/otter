package io.taig.otter

object Discriminator:
  final case class Nested(identifier: String, value: String)

  object Nested:
    val Default: Discriminator.Nested = Nested(identifier = "type", value = "value")

  final case class Merged(identifier: String)

  object Merged:
    val Default: Discriminator.Merged = Merged(identifier = "type")
