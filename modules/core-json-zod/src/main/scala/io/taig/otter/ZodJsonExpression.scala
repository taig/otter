package io.taig.otter

import cats.Show

enum ZodJsonExpression:
  case Inline(expression: String)
  case Reference(name: String)

  final override def toString: String = this match
    case Inline(expression) => expression
    case Reference(name)    => name

object ZodJsonExpression:
  given Show[ZodJsonExpression] = Show.fromToString
