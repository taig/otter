package io.taig.otter

import cats.data.{Chain, Validated}
import io.taig.otter.validation.{Constraint, Validation, Violation, Violations}

abstract class Value[A] extends Schema[A]:
  self =>
  override type Self[a] <: Value[a]

  def print(a: A): Option[String]
  def parse(value: Option[String]): Validated[Violations, A]
