package io.taig.otter

final case class Violation(constraint: Constraint, actual: Data):
  def print: String = s"[${constraint.print}] ! ${actual.print}"

  override def toString: String = print
