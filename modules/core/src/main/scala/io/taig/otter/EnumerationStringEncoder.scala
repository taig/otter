package io.taig.otter

import io.taig.otter.Plain.*

object EnumerationStringEncoder:
  def apply[A](schema: Enumeration.Writer[A], a: A): Option[String] = ???
