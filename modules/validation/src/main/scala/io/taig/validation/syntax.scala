package io.taig.validation

import io.taig.validation.Constraint

object syntax:
  extension [A](self: Constraint[A]) def toViolation[B](actual: B): Violation[A, B] = Violation(self, actual)

  extension (self: Constraint.Identifier)
    def toConstraint[A](reference: Option[A]): Constraint[A] =
      Constraint.Rule(self, reference, Constraint.Type.Universal)
    def toConstraint[A](reference: Option[A], equal: Boolean, delta: Option[A]): Constraint[A] =
      Constraint.Rule(self, reference, Constraint.Type.Numeric(equal, delta))
