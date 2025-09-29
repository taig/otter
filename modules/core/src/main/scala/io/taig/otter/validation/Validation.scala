package io.taig.otter.validation

import cats.data.ValidatedNec
import cats.Invariant
import cats.data.Validated

abstract class Validation[+C <: Constraint, I, O]:
  self =>

  def decode(input: I): ValidatedNec[Violation[C], O]

  def encode(o: O): I

  final def imap[T](f: O => T)(g: T => O): Validation[C, I, T] = new Validation[C, I, T]:
    override def decode(input: I): ValidatedNec[Violation[C], T] = self.decode(input).map(f)
    override def encode(o: T): I = self.encode(g(o))

object Validation:
  def valid[A]: Validation[Nothing, A, A] = new Validation[Nothing, A, A]:
    override def decode(a: A): ValidatedNec[Violation[Nothing], A] = Validated.valid(a)
    override def encode(a: A): A = a

  given [C <: Constraint, I]: Invariant[Validation[C, I, *]] with
    override def imap[A, B](fa: Validation[C, I, A])(f: A => B)(g: B => A): Validation[C, I, B] =
      fa.imap(f)(g)
