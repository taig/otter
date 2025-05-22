package io.taig.otter

import cats.Eq
import cats.Show
import cats.derived.*
import cats.syntax.all.*

enum ZodExpression derives Eq:
  case Inline(value: String)
  case Referenced(reference: ZodConst, value: String)

object ZodExpression:
  def apply(name: String, value: String): ZodExpression.Referenced =
    Referenced(reference = ZodConst(name), value)

  given Show[ZodExpression] =
    case Inline(value)            => value
    case Referenced(reference, _) => reference.show
