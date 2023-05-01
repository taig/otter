package io.taig.openapi.schema

import cats.data.Validated
import io.taig.openapi.OpenApi

abstract class Value[A] extends Schema[A]:
  self =>
  override type Self[a] <: Value[a] { type Self[a] = self.Self[a] }
  final override type Codec = OpenApi.Primitive

  def parse(value: String): Validated[Violations, A]

  def render(a: A): String
