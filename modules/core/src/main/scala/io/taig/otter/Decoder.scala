package io.taig.otter

import cats.data.Validated
import io.taig.otter.validation.Violations

trait Decoder[S[_], A]:
  def apply[B](schema: S[B], a: A): Decoder.Result[A, B]

object Decoder:
  type Result[A, B] = Validated[Violations[Constraint.Any[A], A], B]
