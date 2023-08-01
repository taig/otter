package io.taig.crock.validation

import java.util.regex.Pattern

enum Constraint:
  case MinLength(reference: Int)
  case MaxLength(reference: Int)
  case Matches(pattern: Pattern)
  case Minimum(reference: BigDecimal, exclusive: Boolean)
  case Maximum(reference: BigDecimal, exclusive: Boolean)
  case Multiple(reference: BigDecimal)
  case Type(name: String)
  case Custom(identifier: String, reference: Option[String])
