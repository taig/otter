package io.taig.otter.sample.tapir

import sttp.tapir.{Schema, SchemaType}

opaque type Isbn = Long

object Isbn:
  implicit val schema: Schema[Isbn] = Schema(SchemaType.SInteger())

//object Isbn:
//  extension (self: Isbn) def toLong: Long = self
//  def unsafeFromLong(value: Long): Isbn = value
//  val validation: Validation[Long, Isbn] = ???
////  (
////    validations.minimum(1000000000000L) *>
////      validations.maximum(9999999999999L)
////  ).tap
//
//  given (using order: Order[Long]): Order[Isbn] = order
