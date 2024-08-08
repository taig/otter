package io.taig.otter

import cats.Show

final case class Indexed[A](xpath: XPath, self: A)

object Indexed:
  given [A: Show]: Show[Indexed[A]] = ???
