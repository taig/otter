package io.taig.otter.http

import cats.Eq
import cats.Show

opaque type Method = String

object Method:
  inline def apply(value: String): Method = value

  given Eq[Method] = Eq.fromUniversalEquals

  given Show[Method] = Show.show(identity)
