package io.taig.otter.validation

import cats.Bifunctor

final case class Violation[+A, +B](constraint: Constraint[A], actual: B):
  def map[C](f: B => C): Violation[A, C] = copy(actual = f(actual))
  def bimap[C, D](f: A => C, g: B => D): Violation[C, D] = Violation(constraint.map(f), g(actual))

object Violation:
  def maxItems(reference: Long, actual: Long): Violation[Nothing, Long] =
    Violation(Constraint.MaxItems(reference), actual)
  def minItems(reference: Long, actual: Long): Violation[Nothing, Long] =
    Violation(Constraint.MinItems(reference), actual)
  def tpe(name: String, actual: String): Violation[Nothing, String] = Violation(Constraint.Type(name), actual)

  given Bifunctor[Violation] with
    override def bimap[A, B, C, D](fab: Violation[A, B])(f: A => C, g: B => D): Violation[C, D] = fab.bimap(f, g)
