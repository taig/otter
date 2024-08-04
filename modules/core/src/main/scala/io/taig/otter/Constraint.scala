package io.taig.otter

import java.util.regex.Pattern
import cats.Functor
import scala.Product as SProduct

sealed abstract class Constraint[+A] extends SProduct with Serializable

object Constraint:
  final case class Type(name: String) extends Constraint[Nothing]

  sealed abstract class Collection extends Constraint[Data.Array[?]]

  object Collection:
    final case class MaxItems(reference: Long) extends Constraint.Collection
    final case class MinItems(reference: Long) extends Constraint.Collection
    case object UniqueItems extends Constraint.Collection

  sealed abstract class Object extends Constraint[Data.Object[?]]

  object Object:
    final case class MaxProperties(reference: Long) extends Constraint.Object
    final case class MinProperties(reference: Long) extends Constraint.Object

  sealed abstract class Primitive extends Constraint[Data.Primitive]

  object Primitive:
    final case class Matches(pattern: Pattern) extends Primitive
    final case class Maximum(reference: Data.Primitive, exclusive: Boolean) extends Primitive
    final case class Minimum(reference: Data.Primitive, exclusive: Boolean) extends Primitive
    final case class MaxLength(reference: Int) extends Primitive
    final case class MinLength(reference: Int) extends Primitive
    final case class Multiple(reference: Data.Primitive) extends Primitive
    final case class OneOf(values: List[Data.Primitive]) extends Primitive
