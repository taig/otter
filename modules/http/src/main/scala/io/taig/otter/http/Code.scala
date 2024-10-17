package io.taig.otter.http

import cats.Order

opaque type Code = Int

object Code:
  extension (code: Code) inline def toInt: Int = code
  inline def apply(code: Int): Code = code

  given (using order: Order[Int]): Order[Code] = order
