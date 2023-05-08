package io.taig.openapi.http

import cats.Order
import io.taig.openapi.{Encoder, OpenApi}

opaque type Code = Int

object Code:
  extension (code: Code) def toInt: Int = code
  def apply(code: Int): Code = code

  given Encoder[Code] = OpenApi.fromInt(_)
  given (using order: Order[Int]): Order[Code] = order
