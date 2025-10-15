package io.taig.otter.codec

import cats.data.Validated
import io.taig.validation.Violation
import io.taig.otter.Violations

trait Parser[-S[_]] extends Decoder[S, String]:
  def parse[A](schema: S[A], value: String): Validated[Violations, A]

  final override inline def decode[A](schema: S[A], value: String): Validated[Violations, A] = parse(schema, value)
