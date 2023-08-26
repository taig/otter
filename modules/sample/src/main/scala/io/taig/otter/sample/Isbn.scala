package io.taig.otter.sample

import io.taig.otter.validation.Validation

opaque type Isbn = Long

object Isbn:
  extension (self: Isbn) def toLong: Long = self
  def unsafeFromLong(value: Long): Isbn = value
  val validation: Validation[Long, Isbn] =
    ???