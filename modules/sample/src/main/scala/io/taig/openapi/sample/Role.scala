package io.taig.openapi.sample

import cats.Eq

enum Role:
  case Admin
  case Member

object Role:
  given Eq[Role] = Eq.fromUniversalEquals
