package io.taig.otter.sample.api.schema

import io.taig.otter.sample.Dsl.*
import cats.syntax.all.*

opaque type Isbn = Long

object Isbn:
  val codec: Primitive.Required[Isbn] =
    long(minimum = comparison(1000000000000L).some, maximum = comparison(9999999999999L).some)
