package io.taig.otter.sample.api.schema

import io.taig.otter.sample.api.Dsl.*
import cats.syntax.all.*

opaque type IsbnApiSchema = Long

object IsbnApiSchema:
  val codec: Primitive.Required[IsbnApiSchema] =
    long(minimum = comparison(1L).some, maximum = comparison(9999999999999L).some)
