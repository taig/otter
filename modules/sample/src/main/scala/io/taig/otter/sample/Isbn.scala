package io.taig.otter.sample

import cats.Order

opaque type Isbn = Long

object Isbn:
  extension (self: Isbn) def toLong: Long = self
  def apply(value: Long): Isbn = value

  given (using order: Order[Long]): Order[Isbn] = order
