package io.taig.otter.sample.data

import cats.Order
import cats.syntax.all.*
import io.taig.otter.validation.{validations, Validation}

opaque type Isbn = Long

object Isbn:
  extension (self: Isbn) def toLong: Long = self
  def unsafeFromLong(value: Long): Isbn = value
  val validation: Validation[Long, Isbn] = (
    validations.minimum(1000000000000L) *>
      validations.maximum(9999999999999L)
  ).tap

  given (using order: Order[Long]): Order[Isbn] = order
