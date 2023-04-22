package io.taig.openapi.csv

import cats.Eval
import cats.data.Validated
import io.taig.openapi.OpenApi
import io.taig.openapi.schema.{Schema, Violations}
import io.taig.validation.Violation

final case class Cell[A](name: String, schema: Eval[Schema.Of[A, OpenApi.Primitive]]):
  def decode(openapi: OpenApi): Validated[Violations, A] = schema.value.decode(openapi)

  def encode(a: A): OpenApi.Primitive = schema.value.encode(a)
