package io.taig.otter

import io.taig.otter.validation.Validation
import cats.syntax.all.*

trait Transformation[A, +B, +C, D] extends Transformation.Reader[A, B, C, D], Transformation.Writer[A, D]:
  self =>

  final def imap[E](f: D => E)(g: E => D): Transformation[A, B, C, E] = new Transformation:
    override def validation: Validation[A, B, C, E] = self.validation.map(f)
    override def from(e: E): A = self.from(g(e))

object Transformation:
  trait Reader[A, +B, +C, D]:
    self =>

    def validation: Validation[A, B, C, D]

    final def map[E](f: D => E): Transformation.Reader[A, B, C, E] = new Reader:
      override def validation: Validation[A, B, C, E] = self.validation.map(f)

  trait Writer[A, B]:
    self =>

    def from(b: B): A

    final def contramap[C](f: C => B): Transformation.Writer[A, C] = c => self.from(f(c))
