package io.taig.otter

import cats.data.Validated
import io.taig.otter.validation.Violations

abstract class Value[A] extends Schema[A]:
  override type Self[a] <: Value[a]

  def orElse[B](schema: Value[B]): Value[Either[A, B]]

  def print(a: A): Option[String]
  def parse(value: Option[String]): Validated[Violations, A]
