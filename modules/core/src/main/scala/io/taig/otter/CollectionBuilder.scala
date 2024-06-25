package io.taig.otter

import io.taig.otter.validation.Validation
import cats.syntax.all.*

trait CollectionBuilder[A, +B, C] extends CollectionBuilder.Reader[A, B, C], CollectionBuilder.Writer[A, C]:
  self =>

  final def imap[D](f: C => D)(g: D => C): CollectionBuilder[A, B, D] = new CollectionBuilder:
    override def validation: Validation[A, Constraint.Collection, B, D] = self.validation.map(f)
    override def from(d: D): A = self.from(g(d))

object CollectionBuilder:
  trait Reader[A, +B, C]:
    self =>

    def validation: Validation[A, Constraint.Collection, B, C]

    final def map[D](f: C => D): CollectionBuilder.Reader[A, B, D] = new Reader[A, B, D]:
      override def validation: Validation[A, Constraint.Collection, B, D] = self.validation.map(f)

  trait Writer[A, B]:
    self =>

    def from(b: B): A

    final def contramap[C](f: C => B): CollectionBuilder.Writer[A, C] = c => self.from(f(c))
