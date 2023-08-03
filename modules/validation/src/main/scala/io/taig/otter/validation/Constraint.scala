package io.taig.otter.validation

import java.util.regex.Pattern

enum Constraint:
  case Equals(reference: String)
  case MinLength(reference: Int)
  case MaxLength(reference: Int)
  case Matches(pattern: Pattern)
  case Minimum(reference: BigDecimal, exclusive: Boolean)
  case Maximum(reference: BigDecimal, exclusive: Boolean)
  case Multiple(reference: BigDecimal)
  case MinItems(reference: Long)
  case MaxItems(reference: Long)
  case UniqueItems
  case MinProperties(reference: Int)
  case MaxProperties(reference: Int)
  case Type(name: String)
  case OneOf(values: List[String])
  case Required
  case Custom(identifier: String, reference: Option[String])
