package io.taig.otter

import cats.Eq

enum Null:
  case Hide
  case Show

object Null:
  val Default: Null = Show

  given Eq[Null] = Eq.fromUniversalEquals
