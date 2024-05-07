package io.taig.otter.validation

import cats.data.NonEmptyChain
import cats.syntax.all.*
import cats.kernel.Semigroup

enum Violations[+A]:
  case Root(violation: Violation[A])
  case Group(violations: NonEmptyChain[Violations[A]])
  case Namespace(step: History.Step, violations: Violations[A])

  final def /:(step: History.Step): Violations[A] = Namespace(step, this)
  final def /:(index: Int): Violations[A] = /:(History.Step.Index(index))
  final def /:(field: String): Violations[A] = /:(History.Step.Field(field))

  final def combine[A1 >: A](violations: Violations[A1]): Violations[A1] = (this, violations) match
    case (left @ Root(_), right @ Root(_)) => Group(NonEmptyChain(left, right))
    case (Group(xs), Group(ys))            => Group(xs ++ ys)
    case (left @ Namespace(x, xs), right @ Namespace(y, ys)) =>
      if x === y then Namespace(x, xs.combine(ys)) else Group(NonEmptyChain(left, right))
    case (left, Group(ys))                => Group(left +: ys)
    case (Group(xs), right)               => Group(xs :+ right)
    case (left @ Namespace(x, xs), right) => Group(NonEmptyChain(left, right))
    case (left, right @ Namespace(y, ys)) => Group(NonEmptyChain(left, right))

object Violations:
  def root[A](violations: NonEmptyChain[Violation[A]]): Violations[A] = Group(violations.map(Root.apply))
  def root[A](violation: Violation[A]): Violations[A] = Root(violation)

  given [A]: Semigroup[Violations[A]] with
    override def combine(x: Violations[A], y: Violations[A]): Violations[A] = x.combine(y)

// opaque type Violations[+C] = NonEmptyMap[History, NonEmptyChain[Violation[C]]]

// object Violations:
//   extension [C](violations: Violations[C])
//     def toNem: NonEmptyMap[History, NonEmptyChain[Violation[C]]] = violations
//     def modifyHistory(f: History => History): Violations[C] = violations.mapKeys(f)
//     def modifyViolations[D](
//         f: NonEmptyChain[Violation[C]] => NonEmptyChain[Violation[D]]
//     ): Violations[D] = violations.map(f)
//     def modifyViolation[D](f: Violation[C] => Violation[D]): Violations[D] = modifyViolations(_.map(f))
//     infix def merge(right: Violations[C]): Violations[C] = violations |+| right
//     def get(history: History): Chain[Violation[C]] = violations.apply(history).map(_.toChain).orEmpty
//     def head(history: History): Option[Violation[C]] = violations.apply(history).map(_.head)

//   def apply[C](violations: NonEmptyMap[History, NonEmptyChain[Violation[C]]]): Violations[C] = violations

//   def of[C](
//       head: (History, NonEmptyChain[Violation[C]]),
//       tail: (History, NonEmptyChain[Violation[C]])*
//   ): Violations[C] = NonEmptyMap.of(head, tail*)

//   def ofNec[C](head: (History, Violation[C]), tail: (History, Violation[C])*): Violations[C] =
//     NonEmptyMap.of(head.map(NonEmptyChain.one), tail.map(_.map(NonEmptyChain.one))*)

//   def one[C](history: History, violations: NonEmptyChain[Violation[C]]): Violations[C] =
//     NonEmptyMap.one(history, violations)

//   def oneNec[C](history: History, violation: Violation[C]): Violations[C] =
//     one[C](history, NonEmptyChain.one(violation))

//   def root[C](violations: NonEmptyChain[Violation[C]]): Violations[C] = one(History.Root, violations)

//   def rootNec[C](violation: Violation[C]): Violations[C] = oneNec(History.Root, violation)

//   def fromMap[C](values: SortedMap[History, NonEmptyChain[Violation[C]]]): Option[Violations[C]] =
//     NonEmptyMap.fromMap(values)

//   given [C](using semigroup: Semigroup[NonEmptyMap[History, NonEmptyChain[Violation[C]]]]): Semigroup[Violations[C]] =
//     semigroup
