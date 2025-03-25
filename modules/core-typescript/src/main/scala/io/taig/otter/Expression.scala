package io.taig.otter

import cats.Show
import cats.syntax.all.*

enum Expression:
  case Inline(value: String)
  case Referenced(reference: Reference, value: String)

object Expression:
  given Show[Expression] =
    case Inline(value)            => value
    case Referenced(reference, _) => reference.show
