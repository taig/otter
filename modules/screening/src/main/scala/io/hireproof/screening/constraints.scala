package io.taig.screening

import cats.syntax.all.*

object constraints:
  def apply[A](identifier: Constraint.Identifier, reference: A, tpe: Constraint.Type[A]): Constraint[A] =
    Constraint.Rule(identifier, reference.some, tpe)

  def apply[A](identifier: Constraint.Identifier, reference: A): Constraint[A] =
    constraints(identifier, reference, Constraint.Type.Universal)

  def apply(identifier: Constraint.Identifier): Constraint[Nothing] =
    Constraint.Rule(identifier, reference = none, Constraint.Type.Universal)

  def apply[A](identifier: Constraint.Identifier, reference: A, equal: Boolean, delta: Option[A]): Constraint[A] =
    constraints(identifier, reference, Constraint.Type.Numeric(equal, delta))

  object numeric:
    def greaterThan[A](reference: A, equal: Boolean, delta: Option[A]): Constraint[A] =
      constraints(identifiers.numeric.greaterThan, reference, equal, delta)
    def lessThan[A](reference: A, equal: Boolean, delta: Option[A]): Constraint[A] =
      constraints(identifiers.numeric.lessThan, reference, equal, delta)

  object text:
    def atLeast(reference: Int): Constraint[Int] = constraints(identifiers.text.atLeast, reference)
    val required: Constraint[Nothing] = constraints(identifiers.text.required)
