package io.taig.otter

import java.util.regex.Pattern
import cats.data.NonEmptyList
import cats.syntax.all.*
import cats.parse.Parser
import cats.Eq

enum Constraint:
  case Type(name: String)
  case OneOf(values: NonEmptyList[Data.Primitive])

  final def print: String = ConstraintPrinter(this)
  final override def toString: String = print

object Constraint:
  type Any = Constraint | Constraint.Collection | Constraint.Object | Constraint.Primitive

  enum Collection:
    case MaxItems(reference: Int)
    case MinItems(reference: Int)
    case UniqueItems

    final def print: String = ConstraintPrinter(this)
    final override def toString: String = print

  enum Object:
    case MaxProperties(reference: Int)
    case MinProperties(reference: Int)

    final def print: String = ConstraintPrinter(this)
    final override def toString: String = print

  enum Primitive:
    case Matches(pattern: Pattern)
    case Maximum(comparison: Comparison[Data.Number])
    case Minimum(comparison: Comparison[Data.Number])
    case MaxLength(reference: Int)
    case MinLength(reference: Int)
    case Multiple(reference: Data.Number)

    final def print: String = ConstraintPrinter(this)
    final override def toString: String = print

  def parse(value: String): Either[Parser.Error, Constraint.Any] = ConstraintParser(value)

  given Eq[Constraint.Any] = Eq.instance:
    case (Primitive.Matches(x), Primitive.Matches(y)) => x.pattern() === y.pattern()
    case (x, y)                                       => x == y
