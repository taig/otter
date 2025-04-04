package io.taig.otter

import cats.Eq
import cats.Show
import cats.derived.*
import cats.syntax.all.*

enum Expression derives Eq:
  case Inline(value: String)
  case Referenced(reference: Const, value: String)

object Expression:
  given Show[Expression] =
    case Inline(value)            => value
    case Referenced(reference, _) => reference.show
