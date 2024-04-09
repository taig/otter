package io.taig.otter.validation

import cats.data.Chain

import java.util.regex.Pattern

enum Constraint[+A]:
  case Equals(reference: A)
  case MinLength(reference: Int)
  case MaxLength(reference: Int)
  case Matches(pattern: Pattern)
  case Minimum(reference: A, exclusive: Boolean)
  case Maximum(reference: A, exclusive: Boolean)
  case Multiple(reference: A)
  case MinItems(reference: Long)
  case MaxItems(reference: Long)
  case UniqueItems
  case MinProperties(reference: Int)
  case MaxProperties(reference: Int)
  case Type(name: String)
  case OneOf(values: Chain[A])
  case Required

  final def map[B](f: A => B): Constraint[B] = ???
