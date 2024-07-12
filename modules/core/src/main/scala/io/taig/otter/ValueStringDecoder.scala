package io.taig.otter

object ValueStringDecoder:
  def apply[A](schema: Value.Reader.Via[String, A], value: Option[String]): Decoder.Result[Option[String], A] = ???
