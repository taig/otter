package io.taig.validation

final case class Constraint[+Ref](name: String, reference: Option[Ref]):
  def map[Ref2](f: Ref => Ref2): Constraint[Ref2] = copy(reference = reference.map(f))
