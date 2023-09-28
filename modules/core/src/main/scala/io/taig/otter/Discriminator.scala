package io.taig.otter

import cats.Eq

enum Discriminator:
  case Nested(identifier: String, value: String)
  case Merged(identifier: String)
  case Keyed

object Discriminator:
  object Nested:
    val Default: Discriminator.Nested = Nested(identifier = "type", value = "value")

  object Merged:
    val Default: Discriminator.Merged = Merged(identifier = "type")

  val Default: Discriminator = Nested.Default

  given Eq[Discriminator] = Eq.fromUniversalEquals
