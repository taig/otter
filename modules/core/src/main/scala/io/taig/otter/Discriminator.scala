package io.taig.otter

enum Discriminator:
  case Explicit(identifier: String, value: String)
  case Merged(identifier: String)
  case Keyed

object Discriminator:
  object Explicit:
    val Default: Discriminator.Explicit = Explicit(identifier = "type", value = "value")

  object Merged:
    val Default: Discriminator.Merged = Merged(identifier = "type")
