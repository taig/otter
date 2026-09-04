package io.taig.otter.fixture

import cats.syntax.all.*
import io.taig.otter.Constraint
import io.taig.otter.Step
import io.taig.otter.Violations

/** Projections of a violation tree, for saying which constraint failed and where without writing the tree out.
  *
  * A `Violations` is a nested structure whose every field an assertion would otherwise have to spell, which is why the
  * suites that could say so mostly settle for `isInvalid` instead. These are what let the contract say more than that.
  */
object violations:
  def constraints(violations: Violations): List[Constraint] = violations match
    case Violations.Root(values, violations) =>
      violations.toList.map(_.constraint) ++ values.toList.flatMap((_, nested) => constraints(nested))
    case Violations.Namespace(values) => values.toSortedMap.toList.flatMap((_, nested) => constraints(nested))

  def paths(violations: Violations): List[List[Step]] = violations match
    case Violations.Root(values, _) =>
      if values.isEmpty then List(Nil)
      else values.toList.flatMap((step, nested) => paths(nested).map(step :: _))
    case Violations.Namespace(values) =>
      values.toSortedMap.toList.flatMap((step, nested) => paths(nested).map(step :: _))
