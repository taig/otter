package io.taig.otter

import cats.Show

enum ZodTypescriptExpression:
  case Inline(expression: String)
  case Reference(name: String)

  final override def toString: String = this match
    case Inline(expression) => expression
    case Reference(name)    => name

object ZodTypescriptExpression:
  given Show[ZodTypescriptExpression] = Show.fromToString
