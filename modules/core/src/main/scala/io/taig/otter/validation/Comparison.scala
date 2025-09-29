package io.taig.otter.validation

import cats.Eq
import cats.Functor
import cats.Order
import cats.derived.*
import cats.syntax.all.*

final case class Comparison[A](reference: A, exclusive: Boolean) derives Eq, Functor:
  def map[B](f: A => B): Comparison[B] = copy(reference = f(reference))

  def lt(value: A)(using Order[A]): Boolean =
    if exclusive then reference < value else reference <= value

  def gt(value: A)(using Order[A]): Boolean =
    if exclusive then reference > value else reference >= value
