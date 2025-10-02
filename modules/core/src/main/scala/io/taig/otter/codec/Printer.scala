package io.taig.otter.codec

trait Printer[-S[_]] extends Encoder[S, String]:
  def print[A](schema: S[A], a: A): String

  final override inline def encode[A](schema: S[A], a: A): String = print(schema, a)
