package io.taig.openapi.validation

import io.taig.openapi.OpenApi

import java.util.regex.Pattern

enum Constraint:
  case MinLength(reference: Int)
  case MaxLength(reference: Int)
  case Matches(pattern: Pattern)
  case Minimum(reference: OpenApi.Number, exclusive: Boolean)
  case Maximum(reference: OpenApi.Number, exclusive: Boolean)
  case Multiple(of: OpenApi.Number)
  case Type(name: String)
