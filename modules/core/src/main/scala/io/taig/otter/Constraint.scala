package io.taig.otter

import cats.Eq
import cats.Show
import cats.derived.strict.*
import cats.syntax.all.*
import io.taig.data.Data
import io.taig.data.Data.given

import java.util.regex.Pattern
import scala.Product as SProduct

sealed abstract class Constraint extends SProduct, Serializable derives Eq:
  final override def toString: String = this match
    case Constraint.Equal(reference)                  => show"equal \"$reference\""
    case Constraint.OneOf(values)                     => show"oneOf [${values.map(_.show).mkString_(",")}]"
    case Constraint.Required                          => "required"
    case Constraint.Type(name)                        => show"type \"$name\""
    case Constraint.Collection.Maximum(reference)     => show"collection.maximum $reference"
    case Constraint.Collection.Minimum(reference)     => show"collection.minimum $reference"
    case Constraint.Collection.Unique                 => "collection.unique"
    case Constraint.Object.Maximum(reference)         => show"collection.maximum $reference"
    case Constraint.Object.Minimum(reference)         => show"collection.minimum $reference"
    case Constraint.Primitive.String.Matches(pattern) => show"primitive.string.matches \"${pattern.pattern()}\""
    case Constraint.Primitive.Number.Maximum(Comparison(reference, true))  => show"primitive.number.lt $reference"
    case Constraint.Primitive.Number.Maximum(Comparison(reference, false)) => show"primitive.number.lteq $reference"
    case Constraint.Primitive.Number.Minimum(Comparison(reference, true))  => show"primitive.number.gt $reference"
    case Constraint.Primitive.Number.Minimum(Comparison(reference, false)) => show"primitive.number.gteq $reference"
    case Constraint.Primitive.String.Maximum(reference)                    => show"primitive.string.maximum $reference"
    case Constraint.Primitive.String.Minimum(reference)                    => show"primitive.string.minimum $reference"
    case Constraint.Primitive.Number.Multiple(reference)                   => show"primitive.string.multiple $reference"

object Constraint:
  final case class Equal(reference: Data) extends Constraint
  final case class OneOf(values: List[Data]) extends Constraint
  type Required = Constraint.Required.type
  case object Required extends Constraint
  final case class Type(name: String) extends Constraint

  sealed abstract class Collection extends Constraint derives Eq

  object Collection:
    final case class Maximum(reference: Int) extends Constraint.Collection
    final case class Minimum(reference: Int) extends Constraint.Collection
    case object Unique extends Constraint.Collection

  sealed abstract class Object extends Constraint derives Eq

  object Object:
    final case class Maximum(reference: Int) extends Constraint.Object
    final case class Minimum(reference: Int) extends Constraint.Object

  sealed abstract class Primitive extends Constraint derives Eq

  object Primitive:
    sealed abstract class Number extends Constraint.Primitive derives Eq

    object Number:
      final case class Maximum(comparison: Comparison[Data.Number]) extends Constraint.Primitive.Number
      final case class Minimum(comparison: Comparison[Data.Number]) extends Constraint.Primitive.Number
      final case class Multiple(reference: Data.Number) extends Constraint.Primitive.Number

    sealed abstract class String extends Constraint.Primitive derives Eq

    object String:
      final case class Matches(pattern: Pattern) extends Constraint.Primitive.String
      final case class Maximum(reference: Int) extends Constraint.Primitive.String
      final case class Minimum(reference: Int) extends Constraint.Primitive.String

  given Show[Constraint] = Show.fromToString
