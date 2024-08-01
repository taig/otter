package io.taig.otter.validation

import cats.data.NonEmptyChain
import cats.syntax.all.*
import cats.Semigroup
import cats.Functor
import cats.data.NonEmptyMap
import cats.data.NonEmptyChainImpl

enum Violations[+A]:
  case Root(violations: NonEmptyChain[A])
  case Namespace(toNem: NonEmptyMap[Option[History.Step], Violations[A]])

  final def /:(step: History.Step): Violations[A] = Namespace(NonEmptyMap.one(step.some, this))
  final def /:(index: Int): Violations[A] = /:(History.Step.Index(index))
  final def /:(field: String): Violations[A] = /:(History.Step.Field(field))

  final def map[B](f: A => B): Violations[B] = this match
    case Root(violations) => Root(violations.map(f))
    case Namespace(toNem) => Namespace(toNem.fmap(_.map(f)))

object Violations:
  def of[A](violation: (History.Step, A), violations: (History.Step, A)*): Violations[A] = Namespace(
    NonEmptyMap.of(violation, violations*).mapBoth { case (key, value) => (key.some, Root(NonEmptyChain.one(value))) }
  )

  def root[A](violations: NonEmptyChain[A]): Violations[A] = Root(violations)
  def rootNec[A](violation: A): Violations[A] = root(NonEmptyChain.one(violation))

  def namespace[A](step: History.Step, violations: NonEmptyChain[A]): Violations[A] =
    Namespace(NonEmptyMap.one(step.some, Root(violations)))
  def namespaceNec[A](step: History.Step, violation: A): Violations[A] =
    namespace(step, NonEmptyChain.one(violation))

  given [A]: Semigroup[Violations[A]] with
    override def combine(x: Violations[A], y: Violations[A]): Violations[A] = (x, y) match
      case (Root(left), Root(right))           => Root(left.concat(right))
      case (Namespace(left), Namespace(right)) => Namespace(left |+| right)
      case (left @ Root(_), Namespace(right))  => Namespace(right |+| NonEmptyMap.one(none, left))
      case (Namespace(left), right @ Root(_))  => Namespace(left |+| NonEmptyMap.one(none, right))

  given Functor[Violations] with
    override def map[A, B](fa: Violations[A])(f: A => B): Violations[B] = fa.map(f)
