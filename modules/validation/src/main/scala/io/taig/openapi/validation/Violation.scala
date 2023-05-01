package io.taig.openapi.validation

final case class Violation[+Ref, +Act](constraint: Constraint[Ref], actual: Act):
  def modifyConstraint[Ref2](f: Constraint[Ref] => Constraint[Ref2]): Violation[Ref2, Act] =
    copy(constraint = f(constraint))
  def mapReference[Ref2](f: Ref => Ref2): Violation[Ref2, Act] = modifyConstraint(_.map(f))
  def mapActual[Act2](f: Act => Act2): Violation[Ref, Act2] = copy(actual = f(actual))
  def withActual[Act2](actual: Act2): Violation[Ref, Act2] = mapActual(_ => actual)
