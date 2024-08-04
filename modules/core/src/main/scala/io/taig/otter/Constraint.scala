package io.taig.otter

import java.util.regex.Pattern

enum Constraint:
  case Type(name: String)
  case OneOf(values: List[Data.Primitive])

object Constraint:
  type Any = Constraint | Constraint.Collection | Constraint.Object | Constraint.Primitive

  enum Collection:
    case MaxItems(reference: Int)
    case MinItems(reference: Int)
    case UniqueItems

  enum Object:
    case MaxProperties(reference: Int)
    case MinProperties(reference: Int)

  enum Primitive:
    case Matches(pattern: Pattern)
    case Maximum(comparison: Comparison[Data.Number])
    case Minimum(comparison: Comparison[Data.Number])
    case MaxLength(reference: Int)
    case MinLength(reference: Int)
    case Multiple(reference: Data.Number)
