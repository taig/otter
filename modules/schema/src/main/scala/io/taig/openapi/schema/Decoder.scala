package io.taig.openapi.schema

import cats.data.Validated

abstract class Decoder[F[_], A]:
  def decode[B](fa: F[B], a: A): Validated[Violations, B]
