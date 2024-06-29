package io.taig.otter.http

import io.taig.otter.validation.Violations

final class ViolationsException(val violations: Violations[?, ?]) extends RuntimeException:
  override def getMessage: String = ???
  // violations.toNem.toNel
  // .map { case (history, violation) => s"${history.toJsonPath}: $violation" }
  // .mkString_("\n  ", "\n  ", "")
