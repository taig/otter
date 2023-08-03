package io.taig.otter.schema

import cats.Eq

enum Null:
  case Show
  case Hide

object Null:
  val Default: Null = Show
  given Eq[Null] = Eq.fromUniversalEquals
