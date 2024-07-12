package io.taig.otter

object EnumerationRequiredStringDecoder:
  def apply[A](schema: Enumeration.Required.Reader.Via[String, A], value: String): Decoder.Result[Option[String], A] =
    ???
