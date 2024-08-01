package io.taig.otter.sample.data

import cats.Order
import cats.syntax.all.*
import io.taig.otter.http.Dsl.*

opaque type Isbn = Long

object Isbn:
  extension (self: Isbn) def toLong: Long = self

  def unsafe(value: Long): Isbn = value

  val validation: CodecValidation.Primitive[Long, Isbn] =
    (minimum(1000000000000L) *> maximum(9999999999999L)).tap

  given (using order: Order[Long]): Order[Isbn] = order
