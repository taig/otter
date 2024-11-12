package io.taig.otter

enum Discriminator:
  case Nested(identifier: String, value: String)
  case Merged(identifier: String)
  case Keyed

object Discriminator:
  object Nested:
    val Default: Discriminator.Nested = Nested(identifier = "type", value = "value")

  object Merged:
    val Default: Discriminator.Merged = Merged(identifier = "type")
