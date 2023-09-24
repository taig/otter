package io.taig.otter.validation

import cats.data.Chain
import io.taig.otter.Data

import java.util.regex.Pattern

enum Constraint:
  case Equals(reference: String)
  case MinLength(reference: Int)
  case MaxLength(reference: Int)
  case Matches(pattern: Pattern)
  case Minimum(reference: Data.Number, exclusive: Boolean)
  case Maximum(reference: Data.Number, exclusive: Boolean)
  case Multiple(reference: Data.Number)
  case MinItems(reference: Long)
  case MaxItems(reference: Long)
  case UniqueItems
  case MinProperties(reference: Int)
  case MaxProperties(reference: Int)
  case Type(name: String)
  case OneOf(values: Chain[String])
  case Required
