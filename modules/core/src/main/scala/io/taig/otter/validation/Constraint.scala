package io.taig.otter.validation

import cats.data.Chain

import java.util.regex.Pattern

enum Constraint[+A]:
  case Equals(reference: A)
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
  case OneOf(values: Chain[A])
  case Required
  case Type(name: String)
  case UniqueItems

  final def map[B](f: A => B): Constraint[B] = this match
    case Equals(reference)             => Equals(f(reference))
    case Maximum(reference, exclusive) => Maximum(f(reference), exclusive)
    case MaxLength(reference)          => MaxLength(reference)
    case Minimum(reference, exclusive) => Minimum(f(reference), exclusive)
    case MinLength(reference)          => MinLength(reference)
    case Multiple(reference)           => Multiple(f(reference))
    case OneOf(values)                 => OneOf(values.map(f))
    case _                             => ???
