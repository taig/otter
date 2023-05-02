package io.taig.openapi.http

import cats.Order

opaque type Code = Int

object Code:
  extension (code: Code) def toInt: Int = code

  def apply(code: Int): Code = code

  given (using order: Order[Int]): Order[Code] = order
