package io.taig.otter.validation

final case class Violation[+A](constraint: Constraint[A], actual: A):
  def map[B](f: A => B): Violation[B] = Violation(constraint.map(f), f(actual))
