package io.taig.otter.sample.api.schema

import io.taig.otter.sample.api.Dsl.*
import cats.syntax.all.*

opaque type IsbnApiSchema = Long

object IsbnApiSchema:
  extension (self: IsbnApiSchema) def toLong: Long = self

  def unsafe(value: Long): IsbnApiSchema = value

  val codec: Primitive.Required[IsbnApiSchema] =
    long(minimum = comparison(1L).some, maximum = comparison(9999999999999L).some)
