package io.taig.otter

import cats.Show
import cats.syntax.all.*
import cats.Eq
import cats.derived.*

enum Expression derives Eq:
  case Inline(value: String)
  case Referenced(reference: Const, value: String)

object Expression:
  given Show[Expression] =
    case Inline(value)            => value
    case Referenced(reference, _) => reference.show
