package io.taig.otter.http

import cats.data.Validated
import io.taig.otter.Violations

object FromDataKeyParser:
  def apply[A](codec: FormData.Key[A], value: String): Validated[Violations, A] = ???
