package io.taig.otter

import io.taig.otter.Plain.*

object EnumerationRequiredStringEncoder:
  def apply[A](schema: Enumeration.Required.Writer[A], a: A): String = ???
