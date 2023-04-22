package io.taig.screening

import cats.Applicative
import cats.syntax.all.*

final case class Violation[+A, +B](constraint: Constraint[A], actual: B):
  def modifyConstraint[T](f: Constraint[A] => Constraint[T]): Violation[T, B] = copy(constraint = f(constraint))
  def mapConstraint[T](f: A => T): Violation[T, B] = modifyConstraint(_.map(f))
  def mapActual[T](f: B => T): Violation[A, T] = copy(actual = f(actual))
  def traverseActual[F[+_]: Applicative, T](f: B => F[T]): F[Violation[A, T]] =
    f(actual).map(actual => copy(actual = actual))
  def withActual[T](actual: T): Violation[A, T] = mapActual(_ => actual)
