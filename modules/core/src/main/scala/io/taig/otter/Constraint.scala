package io.taig.otter

import cats.Eq
import cats.Show
import cats.derived.strict.*
import cats.syntax.all.*
import io.taig.otter.Data.given

import java.util.regex.Pattern
import scala.Product as SProduct

sealed abstract class Constraint extends SProduct, Serializable derives Eq:
  final override def toString: String = this match
    case Constraint.Equal(reference) => show"equal \"$reference\""
    case Constraint.OneOf(values) =>
      show"oneOf [${values.map(Data.Primitive.show.show).mkString_(",")}]"
    case Constraint.Required                                        => "required"
    case Constraint.Type(name)                                      => show"type \"$name\""
    case Constraint.Collection.MaxItems(reference)                  => show"maxItem $reference"
    case Constraint.Collection.MinItems(reference)                  => show"minItem $reference"
    case Constraint.Collection.UniqueItems                          => "uniqueItems"
    case Constraint.Object.MaxProperties(reference)                 => show"maxProperties $reference"
    case Constraint.Object.MinProperties(reference)                 => show"minProperties $reference"
    case Constraint.Primitive.Matches(pattern)                      => show"matches \"${pattern.pattern()}\""
    case Constraint.Primitive.Maximum(Comparison(reference, true))  => show"lt $reference"
    case Constraint.Primitive.Maximum(Comparison(reference, false)) => show"lteq $reference"
    case Constraint.Primitive.Minimum(Comparison(reference, true))  => show"gt $reference"
    case Constraint.Primitive.Minimum(Comparison(reference, false)) => show"gteq $reference"
    case Constraint.Primitive.MaxLength(reference)                  => show"maxLength $reference"
    case Constraint.Primitive.MinLength(reference)                  => show"minLength $reference"
    case Constraint.Primitive.Multiple(reference)                   => show"multiple $reference"

object Constraint:
  final case class Equal(reference: Data.Any) extends Constraint
  final case class OneOf(values: List[Data.Primitive]) extends Constraint
  case object Required extends Constraint
  final case class Type(name: String) extends Constraint

  sealed abstract class Collection extends Constraint derives Eq

  object Collection:
    final case class MaxItems(reference: Int) extends Constraint.Collection
    final case class MinItems(reference: Int) extends Constraint.Collection
    case object UniqueItems extends Constraint.Collection

  sealed abstract class Object extends Constraint derives Eq

  object Object:
    final case class MaxProperties(reference: Int) extends Constraint.Object
    final case class MinProperties(reference: Int) extends Constraint.Object

  sealed abstract class Primitive extends Constraint derives Eq

  object Primitive:
    final case class Matches(pattern: Pattern) extends Constraint.Primitive
    final case class Maximum(comparison: Comparison[Data.Number]) extends Constraint.Primitive
    final case class Minimum(comparison: Comparison[Data.Number]) extends Constraint.Primitive
    final case class MaxLength(reference: Int) extends Constraint.Primitive
    final case class MinLength(reference: Int) extends Constraint.Primitive
    final case class Multiple(reference: Data.Number) extends Constraint.Primitive

  given Show[Constraint] = Show.fromToString
