package io.taig.otter

final case class Violation(constraint: Constraint.Any, actual: Data):
  def print: String = s"[$constraint] ! TODO"

  override def toString: String = print
