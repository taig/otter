package io.taig.otter.http

import cats.Semigroup
import cats.syntax.all.*
import io.taig.otter.Violations

/** A decoding failure always carries the violation tree required by the core decoder contract. */
final case class DecodingFailure(
    category: Failure.Category,
    violations: Violations,
    cause: Option[Throwable] = None
):
  def failure: Failure = Failure(category, Some(violations), cause)

object DecodingFailure:
  /** Eligible body alternatives take precedence over content-type mismatches from ineligible alternatives. */
  given Semigroup[DecodingFailure]:
    override def combine(left: DecodingFailure, right: DecodingFailure): DecodingFailure =
      def priority(category: Failure.Category): Int = category match
        case Failure.Category.Interpreter => 0
        case Failure.Category.Envelope    => 1
        case Failure.Category.Syntax      => 2
        case Failure.Category.Validation  => 3
        case Failure.Category.ContentType => 4
        case _                            => 5
      val selected = if priority(left.category) <= priority(right.category) then left else right
      selected.copy(violations = left.violations |+| right.violations)
