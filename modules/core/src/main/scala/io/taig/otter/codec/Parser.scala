package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Violation

trait Parser[-S[_]] extends Decoder[S, String]:
  def parse[A](schema: S[A], value: String): Validated[Violation, A]

  final override inline def decode[A](schema: S[A], value: String): Validated[Violation, A] = parse(schema, value)
