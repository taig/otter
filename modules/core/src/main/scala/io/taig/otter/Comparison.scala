package io.taig.otter

import cats.Eq
import cats.Functor
import cats.derived.*

final case class Comparison[A](reference: A, exclusive: Boolean) derives Eq, Functor:
  def map[B](f: A => B): Comparison[B] = copy(reference = f(reference))
