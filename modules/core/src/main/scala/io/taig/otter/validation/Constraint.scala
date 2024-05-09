package io.taig.otter.validation

import java.util.regex.Pattern

enum Constraint[+A]:
  case Matches(pattern: Pattern)
  case Maximum(reference: A, exclusive: Boolean)
  case MaxItems(reference: Long)
  case MaxLength(reference: Int)
  case MaxProperties(reference: Int)
  case Minimum(reference: A, exclusive: Boolean)
  case MinItems(reference: Long)
  case MinLength(reference: Int)
  case MinProperties(reference: Int)
  case Multiple(reference: A)
  case Type(name: String)
  case UniqueItems

  def map[B](f: A => B): Constraint[B] = ???
