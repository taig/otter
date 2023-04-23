package io.taig.validation

final case class Constraint[+Ref](name: String, reference: Option[Ref])
