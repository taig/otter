package io.taig.openapi.schema

import cats.Eq

sealed trait Void extends Serializable

object Void extends Void:
  given eq: Eq[Void] = Eq.fromUniversalEquals
