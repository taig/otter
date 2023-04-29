package io.taig.openapi.validation

final case class Constraint[+Ref](name: String, reference: Option[Ref]):
  def map[Ref2](f: Ref => Ref2): Constraint[Ref2] = copy(reference = reference.map(f))

object Constraint:
  def withoutReference(name: String): Constraint[Nothing] = Constraint(name, None)
  def withReference[Ref](name: String, reference: Ref): Constraint[Ref] = Constraint(name, Some(reference))
