package io.taig.validation

final case class Violation[+Ref, +Act](constraint: Constraint[Ref], actual: Act)
