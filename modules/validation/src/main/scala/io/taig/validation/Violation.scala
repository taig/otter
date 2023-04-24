package io.taig.validation

final case class Violation[+Ref, +Act](constraint: Constraint[Ref], actual: Act):
  def mapReference[Ref2](f: Ref => Ref2): Violation[Ref2, Act] = copy(constraint = constraint.map(f))
