package io.taig.otter

import java.util.regex.Pattern
import cats.data.NonEmptyList

enum Constraint:
  case Type(name: String)
  case OneOf(values: NonEmptyList[Data.Primitive])

  final override def toString: String = this match
    case Type(name)    => s"type '$name'"
    case OneOf(values) => s"oneOf ${values.map(_.printQuoted).toList.mkString(",")}"

object Constraint:
  type Any = Constraint | Constraint.Collection | Constraint.Object | Constraint.Primitive

  enum Collection:
    case MaxItems(reference: Int)
    case MinItems(reference: Int)
    case UniqueItems

    final override def toString: String = this match
      case MaxItems(reference: Int) => s"maxItems $reference"
      case MinItems(reference: Int) => s"minItems $reference"
      case UniqueItems              => "uniqueItems"

  enum Object:
    case MaxProperties(reference: Int)
    case MinProperties(reference: Int)

    final override def toString: String = this match
      case MaxProperties(reference) => s"maxProperties $reference"
      case MinProperties(reference) => s"minProperties $reference"

  enum Primitive:
    case Matches(pattern: Pattern)
    case Maximum(comparison: Comparison[Data.Number])
    case Minimum(comparison: Comparison[Data.Number])
    case MaxLength(reference: Int)
    case MinLength(reference: Int)
    case Multiple(reference: Data.Number)

    final override def toString: String = this match
      case Matches(pattern)                      => s"matches '$pattern'"
      case Maximum(Comparison(reference, true))  => s"lt $reference"
      case Maximum(Comparison(reference, false)) => s"lteq $reference"
      case Minimum(Comparison(reference, true))  => s"gt $reference"
      case Minimum(Comparison(reference, false)) => s"gteq $reference"
      case MaxLength(reference)                  => s"maxLength $reference"
      case MinLength(reference)                  => s"minLength $reference"
      case Multiple(reference)                   => s"multiple $reference"

  def parse(value: String): Option[Constraint.Any] =
    ???
