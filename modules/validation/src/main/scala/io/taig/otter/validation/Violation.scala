package io.taig.otter.validation

import cats.Bifunctor

final case class Violation[+A, +B](constraint: A, actual: B):
  def map[C](f: B => C): Violation[A, C] = copy(actual = f(actual))
  def bimap[C, D](f: A => C, g: B => D): Violation[C, D] = Violation(f(constraint), g(actual))

object Violation:
  given Bifunctor[Violation] with
    override def bimap[A, B, C, D](fab: Violation[A, B])(f: A => C, g: B => D): Violation[C, D] = fab.bimap(f, g)
