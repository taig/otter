package io.taig.crock.schema

import cats.data.Validated

abstract class Parser[F[_], A]:
  def parse[B](fa: F[B], a: A): Validated[Violations, B]
