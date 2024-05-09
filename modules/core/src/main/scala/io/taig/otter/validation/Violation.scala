package io.taig.otter.validation

import cats.Bifunctor

final case class Violation[+A, +B](constraint: Constraint[A], actual: B):
  def bimap[C, D](f: A => C, g: B => D): Violation[C, D] = Violation(constraint.map(f), g(actual))

object Violation:
  given Bifunctor[Violation] with
    override def bimap[A, B, C, D](fab: Violation[A, B])(f: A => C, g: B => D): Violation[C, D] = fab.bimap(f, g)
