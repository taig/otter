package io.taig.crock.http

import cats.Order

opaque type Code = Int

object Code:
  extension (code: Code) def toInt: Int = code
  def apply(code: Int): Code = code

  given Encoder[Code] = OpenApi.fromInt(_)
  given (using order: Order[Int]): Order[Code] = order
