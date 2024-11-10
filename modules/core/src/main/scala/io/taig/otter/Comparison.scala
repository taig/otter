package io.taig.otter

import cats.Functor
import cats.Eq

final case class Comparison[A](reference: A, exclusive: Boolean):
  def map[B](f: A => B): Comparison[B] = copy(reference = f(reference))

object Comparison:
  given Functor[Comparison] with
    override def map[A, B](fa: Comparison[A])(f: A => B): Comparison[B] = fa.map(f)

  given [A: Eq]: Eq[Comparison[A]] = Eq.by(Tuple.fromProductTyped)
