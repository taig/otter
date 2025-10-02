package io.taig.otter.codec

abstract class Printer[-S[_]] extends Encoder[S, String]:
  def print[A](schema: S[A], a: A): String

  override inline def encode[A](schema: S[A], a: A): String = print(schema, a)
