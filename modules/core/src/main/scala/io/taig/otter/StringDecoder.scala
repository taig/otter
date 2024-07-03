package io.taig.otter

import io.taig.otter.Plain.*

object StringDecoder extends Decoder[Value.Required.Reader, String]:
  def apply[A](schema: Value.Required.Reader[A], value: String): Decoder.Result[String, A] = ???
