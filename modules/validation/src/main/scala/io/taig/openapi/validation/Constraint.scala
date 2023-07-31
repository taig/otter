package io.taig.openapi.validation

import java.util.regex.Pattern

enum Constraint:
  case MinLength(reference: Int)
  case MaxLength(reference: Int)
  case Matches(pattern: Pattern)
  case Minimum(reference: BigDecimal, exclusive: Boolean)
  case Maximum(reference: BigDecimal, exclusive: Boolean)
  case Multiple(of: BigDecimal)
  case Type(name: String)
