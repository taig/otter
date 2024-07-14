package io.taig.otter

object ValueStringDecoder:
  def apply[A](schema: Value[?, A], value: Option[String]): Decoder.Result[Option[String], A] = ???
