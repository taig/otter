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

// enum Constraint[+A]:
//   case Type(name: String) extends Constraint[Nothing]
//   case OneOf(values: List[A]) extends Constraint[A]

//   final def map[B](f: A => B): Constraint[B] = this match
//     case Type(name)    => Type(name)
//     case OneOf(values) => OneOf(values.map(f))

// object Constraint:
//   type Any[A] = Constraint[A] | Collection | Object | Primitive[A]

//   sealed abstract class Collection extends SProduct with Serializable

//   object Collection:
//     final case class MaxItems(reference: Long) extends Constraint.Collection
//     final case class MinItems(reference: Long) extends Constraint.Collection
//     case object UniqueItems extends Constraint.Collection

//   enum Object:
//     case MaxProperties(reference: Long)
//     case MinProperties(reference: Long)

//   enum Primitive[+A]:
//     case Matches(pattern: Pattern) extends Primitive[Nothing]
//     case Maximum(reference: A, exclusive: Boolean) extends Primitive[A]
//     case Minimum(reference: A, exclusive: Boolean) extends Primitive[A]
//     case MaxLength(reference: Int) extends Primitive[Nothing]
//     case MinLength(reference: Int) extends Primitive[Nothing]
//     case Multiple(reference: A) extends Primitive[A]

//     final def map[B](f: A => B): Constraint.Primitive[B] = this match
//       case Matches(pattern)              => Matches(pattern)
//       case Maximum(reference, exclusive) => Maximum(f(reference), exclusive)
//       case MaxLength(reference)          => MaxLength(reference)
//       case Minimum(reference, exclusive) => Minimum(f(reference), exclusive)
//       case MinLength(reference)          => MinLength(reference)
//       case Multiple(reference)           => Multiple(f(reference))

//   object Primitive:
//     given Functor[Constraint.Primitive] with
//       override def map[A, B](fa: Constraint.Primitive[A])(f: A => B): Constraint.Primitive[B] = fa.map(f)

  given Functor[Constraint] = ???
