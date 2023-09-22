package io.taig.otter

import cats.data.Validated

trait Decoder[F[_], -A]:
  def decode[B](fa: F[B], a: A): Validated[Violations, B]

object Decoder:
  trait WithRemainders[F[_], A] extends Decoder[F, A]:
    def decodeWithRemainders[B](fa: F[B], a: A): Validated[Violations, (A, B)]
