package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Violations

trait Decoder[-S[_], T]:
  def decode[A](schema: S[A], value: T): Validated[Violations, A]

object Decoder:
  trait Remaining[-S[_], T] extends Decoder[S, T]:
    def decodeRemaining[A](schema: S[A], value: T): Validated[Violations, (T, A)]

    final override def decode[A](schema: S[A], value: T): Validated[Violations, A] =
      decodeRemaining(schema, value).map((_, a) => a)
