package io.taig.otter

import io.taig.otter.validation.Validation
import cats.syntax.all.*

trait Transformation[A, +B, +C, D] extends Transformation.Reader[A, B, C, D], Transformation.Writer[A, D]:
  self =>

  final def imap[E](f: D => E)(g: E => D): Transformation[A, B, C, E] = new Transformation:
    override def validation: Validation[A, B, C, E] = self.validation.map(f)
    override def apply(e: E): A = self.apply(g(e))

  final override def mapValidation[E, F](
      f: Validation[A, B, C, D] => Validation[A, E, F, D]
  ): Transformation[A, E, F, D] =
    new Transformation:
      override def validation: Validation[A, E, F, D] = f(self.validation)
      override def apply(d: D): A = self.apply(d)

object Transformation:
  trait Reader[A, +B, +C, D]:
    self =>

    def validation: Validation[A, B, C, D]

    final def map[E](f: D => E): Transformation.Reader[A, B, C, E] = new Reader:
      override def validation: Validation[A, B, C, E] = self.validation.map(f)

    def mapValidation[E, F](f: Validation[A, B, C, D] => Validation[A, E, F, D]): Transformation.Reader[A, E, F, D] =
      new Reader:
        override def validation: Validation[A, E, F, D] = f(self.validation)

  trait Writer[A, B]:
    self =>

    def apply(b: B): A

    final def contramap[C](f: C => B): Transformation.Writer[A, C] = c => self.apply(f(c))
