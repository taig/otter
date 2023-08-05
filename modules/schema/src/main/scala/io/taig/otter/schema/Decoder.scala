package io.taig.otter.schema

import cats.data.Validated

trait Decoder[F[_], G[_], -A]:
  def decode[B](fa: G[B], a: A): F[Validated[Violations, B]]

object Decoder:
  trait WithRemainders[F[_], G[_], A] extends Decoder[F, G, A]:
    def decodeWithRemainders[B](fa: G[B], a: A): Validated[Violations, (A, B)]
