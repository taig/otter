package io.taig.otter

import io.taig.otter.validation.Validation

trait ValidationOps[Self[_, _], Constraint[_], Actual[_]]:
  extension [A, B](self: Self[A, B])
    def ivalidate[C, D, E, F](validation: Validation[B, Constraint[C], Actual[D], E])(f: E => B): Self[A, E]

object ValidationOps:
  trait Isomorphic[Self[_, _], Constraint[_], Actual[_]] extends ValidationOps[Self, Constraint, Actual]:
    extension [A, B](self: Self[A, B])
      final def ivalidate_[C, D](validation: Validation[B, Constraint[C], Actual[D], Unit]): Self[A, B] =
        self.ivalidate(validation.tap)(identity)

  trait Reader[Self[_, _], Constraint[_], Actual[_]] extends ValidationOps[Self, Constraint, Actual]:
    extension [A, B](self: Self[A, B])
      def validate[C, D, E, F](validation: Validation[B, Constraint[C], Actual[D], E]): Self[A, E]
      final def validate_[C, D](validation: Validation[B, Constraint[C], Actual[D], Unit]): Self[A, B] =
        validate(validation.tap)
      final def ivalidate[C, D, E, F](validation: Validation[B, Constraint[C], Actual[D], E])(f: E => B): Self[A, E] =
        self.validate(validation)

  trait Writer[Self[_, _], Constraint[_], Actual[_]] extends ValidationOps[Self, Constraint, Actual]
