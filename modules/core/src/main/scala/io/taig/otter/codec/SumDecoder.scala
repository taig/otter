package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Sum
import io.taig.otter.Violations

final class SumDecoder[S[_], T, U](branch: Decoder[S, List[(T, U)]]) extends Decoder[Sum[S, *], List[(T, U)]]:
  override def decode[A](schema: Sum[S, A], value: List[(T, U)]): Validated[Violations, A] = ???
