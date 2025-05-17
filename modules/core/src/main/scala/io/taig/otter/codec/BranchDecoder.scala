package io.taig.otter.codec

import io.taig.otter.Branch
import cats.data.Validated
import io.taig.otter.Violations

final class BranchDecoder[S[_], T[_], U](key: Codec[S, String], value: Decoder[T, U])
    extends Decoder[Branch[S, T, *], List[(String, U)]]:
  override def decode[A](schema: Branch[S, T, A], value: List[(String, U)]): Validated[Violations, A] = ???
