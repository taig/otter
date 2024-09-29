package io.taig.otter

import cats.Eq
import cats.Show
import cats.parse.Parser
import cats.syntax.all.*

import java.util.regex.Pattern
import scala.Product as SProduct

sealed abstract class Constraint extends SProduct, Serializable:
  final override def toString: String = Printers(this)

object Constraint:
  final case class Type(name: String) extends Constraint
  final case class OneOf(values: List[Data.Primitive]) extends Constraint

  sealed abstract class Collection extends Constraint

  object Collection:
    final case class MaxItems(reference: Int) extends Constraint.Collection
    final case class MinItems(reference: Int) extends Constraint.Collection
    case object UniqueItems extends Constraint.Collection

  sealed abstract class Object extends Constraint

  object Object:
    final case class MaxProperties(reference: Int) extends Constraint.Object
    final case class MinProperties(reference: Int) extends Constraint.Object

  sealed abstract class Primitive extends Constraint

  object Primitive:
    final case class Matches(pattern: Pattern) extends Constraint.Primitive
    final case class Maximum(comparison: Comparison[Data.Number]) extends Constraint.Primitive
    final case class Minimum(comparison: Comparison[Data.Number]) extends Constraint.Primitive
    final case class MaxLength(reference: Int) extends Constraint.Primitive
    final case class MinLength(reference: Int) extends Constraint.Primitive
    final case class Multiple(reference: Data.Number) extends Constraint.Primitive

  def parse(value: String): Either[Parser.Error, Constraint] = Parsers.constraint.parseAll(value)

  given Eq[Constraint] = Eq.instance:
    case (Primitive.Matches(x), Primitive.Matches(y)) => x.pattern() === y.pattern()
    case (x, y)                                       => x == y

  given Show[Constraint] = Printers(_)
