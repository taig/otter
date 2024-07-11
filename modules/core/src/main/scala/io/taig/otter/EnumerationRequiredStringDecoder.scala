package io.taig.otter

import io.taig.otter.Plain.*

object EnumerationRequiredStringDecoder:
  def apply[A](schema: Enumeration.Required.Reader.Via[String, A], value: String): Decoder.Result[Option[String], A] = ???
