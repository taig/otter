package io.taig.screening

import cats.data.Chain
import cats.syntax.all.*
import cats.{Functor, Order}

import java.time.ZonedDateTime
import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex

enum Constraint[+A]:
  case Or[A, B](left: Chain[Constraint[A]], right: Chain[Constraint[B]]) extends Constraint[A | B]
  case Rule(identifier: Constraint.Identifier, reference: Option[A], tpe: Constraint.Type[A])

  final def map[B](f: A => B): Constraint[B] = this match
    case Or(left, right)                  => Or(left.map(_.map(f)), right.map(_.map(f)))
    case Rule(identifier, reference, tpe) => Rule(identifier, reference.map(f), tpe.map(f))

  final def toRules: Chain[Constraint.Rule[A]] = this match
    case Or(left, right) => left.flatMap(_.toRules) ++ right.flatMap(_.toRules)
    case rule: Rule[?]   => Chain.one(rule)

object Constraint:
  opaque type Identifier = String

  object Identifier:
    extension (self: Constraint.Identifier) def toString: String = self

    def apply(value: String): Identifier = value

    given (using order: Order[String]): Order[Identifier] = order

  enum Type[+A]:
    case Universal extends Type[Nothing]
    case Numeric(equal: Boolean, delta: Option[A])

    final def map[B](f: A => B): Constraint.Type[B] = this match
      case Type.Universal             => Type.Universal
      case Type.Numeric(equal, delta) => Type.Numeric(equal, delta.map(f))

  object Type:
    given Functor[Constraint.Type] with
      override def map[A, B](fa: Constraint.Type[A])(f: A => B): Constraint.Type[B] = fa.map(f)

  given Functor[Constraint] with
    override def map[A, B](fa: Constraint[A])(f: A => B): Constraint[B] = fa.map(f)
