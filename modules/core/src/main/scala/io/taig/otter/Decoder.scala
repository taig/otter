package io.taig.otter

import cats.data.Validated
import io.taig.otter.validation.Violations

trait Decoder[S[_], A, B]:
  def apply[C](schema: S[C], b: B): Decoder.Result[A, C]

object Decoder:
  type Result[A, B] = Validated[Violations[Constraint.Any[A], A], B]
