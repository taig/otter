package io.taig.otter

import cats.data.Validated

abstract class Decoder[S[_], T]:
  def apply[A](codec: S[A], value: T): Validated[Violations, A]
