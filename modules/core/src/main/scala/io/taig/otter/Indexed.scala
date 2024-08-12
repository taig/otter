package io.taig.otter

final case class Indexed[A](xpath: XPath, self: A)
