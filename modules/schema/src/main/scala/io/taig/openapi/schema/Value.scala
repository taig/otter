package io.taig.openapi.schema

import cats.data.{Chain, Validated}
import io.taig.openapi.OpenApi
import io.taig.openapi.validation.{Constraint, Validation}

abstract class Value[A] extends Schema[A]:
  self =>
  override type Self[a] <: Value[a] { type Self[a] = self.Self[a] }
  final override type Codec = OpenApi.Primitive

  def parse(value: String): Validated[Violations, A]

  def render(a: A): String
