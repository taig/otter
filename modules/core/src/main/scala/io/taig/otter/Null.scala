package io.taig.otter

import cats.Eq

enum Null:
  case Show
  case Hide

object Null:
  val Default: Null = Show
  given Eq[Null] = Eq.fromUniversalEquals
