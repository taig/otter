package io.taig.otter.validation

import cats.Order

enum Step:
  case Field(name: String)
  case Index(value: Int)

  final override def toString: String = this match
    case Index(value) => s"[$value]"
    case Field(name)  => name

object Step:
  given Order[Step] =
    case (Field(x), Field(y)) => x compare y
    case (Index(x), Index(y)) => x compare y
    case (Field(_), Index(_)) => 1
    case (Index(_), Field(_)) => -1
