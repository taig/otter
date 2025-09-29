package io.taig.otter.validation

import cats.data.ValidatedNec

abstract class Validation[I, O]:
  def decode(input: I): ValidatedNec[Violation[Constraint], O]

  def encode(o: O): I

  def imap[T](f: O => T)(g: T => O): Validation[I, T]

object Validation:
  abstract class Text[I, O] extends Validation[I, O]:
    self =>

    override def decode(input: I): ValidatedNec[Violation[Constraint.Primitive.String], O]

    final override def imap[T](f: O => T)(g: T => O): Validation.Text[I, T] = new Text[I, T]:
      override def decode(input: I): ValidatedNec[Violation[Constraint.Primitive.String], T] =
        self.decode(input).map(f)
      override def encode(o: T): I = self.encode(g(o))
