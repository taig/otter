package io.taig.otter

import io.taig.otter.Plain.*

object UnionValueRequiredStringDecoder:
  def apply[A](union: Union.Value.Required.Reader[A], value: String): Decoder.Result[Option[String], A] = ???
