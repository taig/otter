package io.taig.otter.validation

import cats.data.NonEmptyChain
import cats.syntax.all.*
import cats.Semigroup
import cats.Functor

enum Violations[+A]:
  case Root(violation: A)
  case Group(violations: NonEmptyChain[Violations[A]])
  case Namespace(step: History.Step, violations: Violations[A])

  final def /:(step: History.Step): Violations[A] = Namespace(step, this)
  final def /:(index: Int): Violations[A] = /:(History.Step.Index(index))
  final def /:(field: String): Violations[A] = /:(History.Step.Field(field))

  final def map[B](f: A => B): Violations[B] = this match
    case Root(violation)             => Root(f(violation))
    case Group(violations)           => Group(violations.map(_.map(f)))
    case Namespace(step, violations) => Namespace(step, violations.map(f))

  final infix def combine[A1 >: A](violations: Violations[A1]): Violations[A1] =
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
  def root[A](violations: NonEmptyChain[A]): Violations[A] = Group(violations.map(Root.apply))
  def rootNec[A](violation: A): Violations[A] = Root(violation)

  def namespace[A](step: History.Step, violations: NonEmptyChain[Violations[A]]) =
    Namespace(step, Group(violations))
  def namespaceNec[A](step: History.Step, violation: A): Violations[A] =
    Namespace(step, Root(violation))

  given [A]: Semigroup[Violations[A]] with
    override def combine(x: Violations[A], y: Violations[A]): Violations[A] = x.combine(y)

  given Functor[Violations] with
    override def map[A, B](fa: Violations[A])(f: A => B): Violations[B] = fa.map(f)
