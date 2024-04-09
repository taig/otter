package io.taig.otter.validation

final case class Violation[+A](constraint: Constraint[A], actual: A):
  def map[B](f: A => B): Violation[B] = Violation(constraint.map(f), f(actual))

// object Violation:
//   def tpe(name: String, actual: Data): Violation = Violation(Constraint.Type(name), actual)
//   def tpe(name: String, actual: String): Violation = tpe(name, Data.String(actual))
//   def tpe(name: String): Violation = tpe(name, actual = Data.Null)
//   val required: Violation = Violation(Constraint.Required, actual = Data.Null)
