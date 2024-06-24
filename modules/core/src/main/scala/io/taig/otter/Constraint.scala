package io.taig.otter

import java.util.regex.Pattern

enum Constraint[+A]:
  case Type(name: String)

object Constraint:
  type Any[A] = Constraint[A] | Primitive[A] | Collection[A] | Object[A]

  enum Primitive[+A]:
    case Matches(pattern: Pattern)
    case MinLength(reference: Int)
    case MaxLength(reference: Int)
    case Minimum(reference: A, exclusive: Boolean)
    case Maximum(reference: A, exclusive: Boolean)
    case Multiple(reference: A)

  enum Collection[+A]:
    case MaxItems(reference: Long)
    case MinItems(reference: Long)
    case UniqueItems

  enum Object[+A]:
    case MinProperties(reference: Int)
    case MaxProperties(reference: Int)
