package io.taig.otter

import java.util.regex.Pattern

enum Constraint:
  case Type(name: String)

object Constraint:
  type Any[A] = Constraint | Primitive[A] | Collection | Object

  enum Primitive[+A]:
    case Matches(pattern: Pattern)
    case MinLength(reference: Int)
    case MaxLength(reference: Int)
    case Minimum(reference: A, exclusive: Boolean)
    case Maximum(reference: A, exclusive: Boolean)
    case Multiple(reference: A)

    final def map[B](f: A => B): Constraint.Primitive[B] = this match
      case Matches(pattern)              => Matches(pattern)
      case MinLength(reference)          => MinLength(reference)
      case MaxLength(reference)          => MaxLength(reference)
      case Minimum(reference, exclusive) => Minimum(f(reference), exclusive)
      case Maximum(reference, exclusive) => Maximum(f(reference), exclusive)
      case Multiple(reference)           => Multiple(f(reference))

  enum Collection:
    case MaxItems(reference: Long)
    case MinItems(reference: Long)
    case UniqueItems

  enum Object:
    case MinProperties(reference: Int)
    case MaxProperties(reference: Int)
