package io.taig.openapi.http

import cats.Order

opaque type Code = Int

object Code:
  def apply(code: Int): Code = code
  extension (code: Code) def toInt: Int = code

  given (using order: Order[Int]): Order[Code] = order
