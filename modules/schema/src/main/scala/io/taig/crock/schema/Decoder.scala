package io.taig.crock.schema

import cats.data.Validated

trait Decoder[F[_], A]:
  def decode[B](fa: F[B], a: A): Validated[Violations, B]
