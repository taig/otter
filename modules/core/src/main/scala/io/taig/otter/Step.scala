package io.taig.otter

import cats.Order
import cats.parse.Parser

enum Step:
  case Field(name: String)
  case Index(value: Int)

  final def print: String = Printers(this)
  final override def toString: String = print

object Step:
  def parse(value: String): Either[Parser.Error, Step] = Parsers.step.parseAll(value)

  given Order[Step] =
    case (Field(x), Field(y)) => x compare y
    case (Index(x), Index(y)) => x compare y
    case (Field(_), Index(_)) => 1
    case (Index(_), Field(_)) => -1
