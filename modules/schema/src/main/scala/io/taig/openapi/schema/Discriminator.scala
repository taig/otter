package io.taig.openapi.schema

import cats.Eq
import cats.syntax.all.*
import cats.Semigroup

enum Discriminator:
  case Nested(identifier: String, value: String)
  case Merged(identifier: String)
  case Keyed
  case None

object Discriminator:
  object Nested:
    val Default: Discriminator.Nested = Nested(identifier = "type", value = "value")

  object Merged:
    val Default: Discriminator.Merged = Merged(identifier = "type")

  val Default: Discriminator = Nested.Default

  given Eq[Discriminator] = Eq.fromUniversalEquals

  given Semigroup[Discriminator] with
    override def combine(x: Discriminator, y: Discriminator): Discriminator = if x === y then x else Default
