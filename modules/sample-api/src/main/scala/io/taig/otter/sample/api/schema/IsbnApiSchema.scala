package io.taig.otter.sample.api.schema

import cats.Order
import io.taig.otter.*
import io.taig.otter.sample.api.dsl.*
import io.taig.otter.sample.api.dsl.json.{comparison as _, *}

opaque type IsbnApiSchema = Long

object IsbnApiSchema:
  extension (self: IsbnApiSchema) def toLong: Long = self

  def apply(value: Long): IsbnApiSchema = value

  val codec: Json.Primitive[IsbnApiSchema] = long(minimum = comparison(1L), maximum = comparison(9999999999999L))

  given (using order: Order[Long]): Order[IsbnApiSchema] = order
