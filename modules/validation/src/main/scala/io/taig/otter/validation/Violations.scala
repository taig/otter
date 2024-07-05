package io.taig.otter.validation

import cats.data.NonEmptyChain
import cats.syntax.all.*
import cats.Semigroup
import cats.Bifunctor
import cats.Functor

enum Violations[+A, +B]:
  case Root(violation: Violation[A, B])
  case Group(violations: NonEmptyChain[Violations[A, B]])
  case Namespace(step: History.Step, violations: Violations[A, B])

  final def /:(step: History.Step): Violations[A, B] = Namespace(step, this)
  final def /:(index: Int): Violations[A, B] = /:(History.Step.Index(index))
  final def /:(field: String): Violations[A, B] = /:(History.Step.Field(field))

  final def bimap[C, D](f: A => C, g: B => D): Violations[C, D] = this match
    case Root(violation)             => Root(violation.bimap(f, g))
    case Group(violations)           => Group(violations.map(_.bimap(f, g)))
    case Namespace(step, violations) => Namespace(step, violations.bimap(f, g))

  final infix def combine[A1 >: A, B1 >: B](violations: Violations[A1, B1]): Violations[A1, B1] =
    (this, violations) match
      case (left @ Root(_), right @ Root(_)) => Group(NonEmptyChain(left, right))
      case (Group(xs), Group(ys))            => Group(xs ++ ys)
      case (left @ Namespace(x, xs), right @ Namespace(y, ys)) =>
        if x === y then Namespace(x, xs.combine(ys)) else Group(NonEmptyChain(left, right))
      case (left, Group(ys))                => Group(left +: ys)
      case (Group(xs), right)               => Group(xs :+ right)
      case (left @ Namespace(x, xs), right) => Group(NonEmptyChain(left, right))
      case (left, right @ Namespace(y, ys)) => Group(NonEmptyChain(left, right))

object Violations:
  def root[A, B](violations: NonEmptyChain[Violation[A, B]]): Violations[A, B] = Group(violations.map(Root.apply))
  def rootNec[A, B](violation: Violation[A, B]): Violations[A, B] = Root(violation)

  def namespace[A, B](step: History.Step, violations: NonEmptyChain[Violations[A, B]]) =
    Namespace(step, Group(violations))
  def namespaceNec[A, B](step: History.Step, violation: Violation[A, B]): Violations[A, B] =
    Namespace(step, Root(violation))

  given [A, B]: Semigroup[Violations[A, B]] with
    override def combine(x: Violations[A, B], y: Violations[A, B]): Violations[A, B] = x.combine(y)

  given bifunctor: Bifunctor[Violations] with
    override def bimap[A, B, C, D](fab: Violations[A, B])(f: A => C, g: B => D): Violations[C, D] =
      fab.bimap(f, g)

  given [A]: Functor[Violations[A, *]] = bifunctor.rightFunctor
