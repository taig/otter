package io.taig.otter.syntax

import io.taig.otter.Absence
import io.taig.otter.Csv
import io.taig.otter.Keys
import io.taig.otter.Tolerance

/** The CSV specific attributes, written against a column rather than a schema.
  *
  * Absence is something only a row's member can do, so these live on [[Csv.Field.Schema]]. What they mean is not what
  * they mean in JSON: a row's columns are fixed by its header, so [[blank]] keeps the column and leaves it empty, and
  * that is the default. [[omitted]] drops the column outright, which shortens the row and only makes sense where
  * nothing has to line up with a header.
  */
trait CsvSyntax:
  extension [S[-w, +r] <: Csv.Cell.Node[w, r], W, R](fa: Csv.Field.Schema[S, W, R])
    def absence(value: Absence): Csv.Field.Schema[S, W, R] = fa.attr(Csv.Namespace, Keys.absence, value)

    def blank: Csv.Field.Schema[S, W, R] = absence(Absence.Empty)

    def omitted: Csv.Field.Schema[S, W, R] = absence(Absence.Omit)

    def tolerance(value: Tolerance): Csv.Field.Schema[S, W, R] = fa.attr(Csv.Namespace, Keys.tolerance, value)

    def strict: Csv.Field.Schema[S, W, R] = tolerance(Tolerance.Strict)

    def lenient: Csv.Field.Schema[S, W, R] = tolerance(Tolerance.Lenient)

object CsvSyntax extends CsvSyntax
