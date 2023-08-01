package io.taig.openapi.validation

import cats.Id

import java.util.regex.Pattern

final case class Constraint2[+Ref, +Act](identifier: String, reference: Ref, schema: Id[Ref], actual: Id[Act])

enum Constraint:
  case MinLength(reference: Int)
  case MaxLength(reference: Int)
  case Matches(pattern: Pattern)
  case Minimum(reference: BigDecimal, exclusive: Boolean)
  case Maximum(reference: BigDecimal, exclusive: Boolean)
  case Multiple(reference: BigDecimal)
  case Type(name: String)
