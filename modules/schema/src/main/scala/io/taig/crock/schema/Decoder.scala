package io.taig.crock.schema

import cats.data.Validated

trait Decoder[F[_], -A]:
  def decode[B](fa: F[B], a: A): Validated[Violations, B]

object Decoder:
  trait WithRemainders[F[_], A] extends Decoder[F, A]:
    override def decode[B](fa: F[B], a: A): Validated[Violations, B] = decodeWithRemainders(fa, a).map(_._2)
    def decodeWithRemainders[B](fa: F[B], a: A): Validated[Violations, (A, B)]
