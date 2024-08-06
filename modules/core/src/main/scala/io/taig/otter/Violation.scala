package io.taig.otter

import cats.Bifunctor
import cats.Functor

final case class Violation[+A, +B](constraint: A, actual: B):
  def map[C](f: B => C): Violation[A, C] = copy(actual = f(actual))
  def bimap[C, D](f: A => C, g: B => D): Violation[C, D] = Violation(f(constraint), g(actual))

  def print: String = s"[$constraint] ! \"$actual\""

  override def toString(): String = print

object Violation:
  given bifunctor: Bifunctor[Violation] with
    override def bimap[A, B, C, D](fab: Violation[A, B])(f: A => C, g: B => D): Violation[C, D] = fab.bimap(f, g)

  given [A]: Functor[Violation[A, *]] = bifunctor.rightFunctor
