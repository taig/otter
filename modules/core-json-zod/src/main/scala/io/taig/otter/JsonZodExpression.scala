package io.taig.otter

import cats.Show

enum JsonZodExpression:
  case Inline(expression: String)
  case Reference(name: String)

  final override def toString: String = this match
    case Inline(expression) => expression
    case Reference(name)    => name

object JsonZodExpression:
  given Show[JsonZodExpression] = Show.fromToString
