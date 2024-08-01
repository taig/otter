package io.taig.otter

import java.util.regex.Pattern
import cats.Functor
import scala.Product as SProduct

enum Constraint[+A]:
  case Type(name: String) extends Constraint[Nothing]
  case OneOf(values: List[A]) extends Constraint[A]

  final def map[B](f: A => B): Constraint[B] = this match
    case Type(name)    => Type(name)
    case OneOf(values) => OneOf(values.map(f))

object Constraint:
  type Any[A] = Constraint[A] | Collection | Object | Primitive[A]

  sealed abstract class Collection extends SProduct with Serializable

  object Collection:
    final case class MaxItems(reference: Long) extends Constraint.Collection
    final case class MinItems(reference: Long) extends Constraint.Collection
    case object UniqueItems extends Constraint.Collection

  enum Object:
    case MaxProperties(reference: Long)
    case MinProperties(reference: Long)

  enum Primitive[+A]:
    case Matches(pattern: Pattern) extends Primitive[Nothing]
    case Maximum(reference: A, exclusive: Boolean) extends Primitive[A]
    case Minimum(reference: A, exclusive: Boolean) extends Primitive[A]
    case MaxLength(reference: Int) extends Primitive[Nothing]
    case MinLength(reference: Int) extends Primitive[Nothing]
    case Multiple(reference: A) extends Primitive[A]

    final def map[B](f: A => B): Constraint.Primitive[B] = this match
      case Matches(pattern)              => Matches(pattern)
      case Maximum(reference, exclusive) => Maximum(f(reference), exclusive)
      case MaxLength(reference)          => MaxLength(reference)
      case Minimum(reference, exclusive) => Minimum(f(reference), exclusive)
      case MinLength(reference)          => MinLength(reference)
      case Multiple(reference)           => Multiple(f(reference))

  object Primitive:
    given Functor[Constraint.Primitive] with
      override def map[A, B](fa: Constraint.Primitive[A])(f: A => B): Constraint.Primitive[B] = fa.map(f)

  given Functor[Constraint.Any] with
    override def map[A, B](fa: Constraint.Any[A])(f: A => B): Constraint.Any[B] = fa match
      case constraint: Constraint[A]           => ???
      case constraint: Constraint.Collection   => constraint
      case constraint: Constraint.Object       => constraint
      case constraint: Constraint.Primitive[A] => constraint.map(f)
