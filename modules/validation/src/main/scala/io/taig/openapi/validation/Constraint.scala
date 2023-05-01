package io.taig.openapi.validation

import cats.syntax.all.*

import scala.util.matching.Regex

final case class Constraint[+Ref](name: String, reference: Option[Ref]):
  def map[Ref2](f: Ref => Ref2): Constraint[Ref2] = copy(reference = reference.map(f))
  def toViolation[Act](actual: Act): Violation[Ref, Act] = Violation(this, actual)

object Constraint:
  def parser(name: String): Constraint[String] = Constraint("parser", name.some)

  def tpe(name: String): Constraint[String] = Constraint("type", name.some)

  val required: Constraint[Nothing] = Constraint("required", none)

  object collection:
    def atLeast(reference: Long): Constraint[Long] = Constraint("collection.atLeast", reference.some)

    def atMost(reference: Long): Constraint[Long] = Constraint("collection.atMost", reference.some)

    def contains[A](reference: A): Constraint[A] = Constraint("collection.contains", reference.some)

    val empty: Constraint[Nothing] = Constraint("collection.empty", none)

    def exactly(reference: Long): Constraint[Long] = Constraint("collection.exactly", reference.some)

    val nonEmpty: Constraint[Nothing] = Constraint("collection.nonEmpty", none)

  object numeric:
    def equal[A](comparison: NumericComparison[A]): Constraint[NumericComparison[A]] =
      Constraint("numeric.equal", comparison.some)

    def greaterThan[A](comparison: NumericComparison[A]): Constraint[NumericComparison[A]] =
      Constraint("numeric.greaterThan", comparison.some)

    def lessThan[A](comparison: NumericComparison[A]): Constraint[NumericComparison[A]] =
      Constraint("numeric.lessThan", comparison.some)

  object text:
    def atLeast(reference: Int): Constraint[Int] = Constraint("text.atLeast", reference.some)

    def atMost(reference: Int): Constraint[Int] = Constraint("text.atMost", reference.some)

    val email: Constraint[Nothing] = Constraint("text.email", none)

    def equal(reference: String): Constraint[String] = Constraint("text.equal", reference.some)

    def exactly(reference: Int): Constraint[Int] = Constraint("text.exactly", reference.some)

    def matches(regex: Regex): Constraint[Regex] = Constraint("text.matches", regex.some)
