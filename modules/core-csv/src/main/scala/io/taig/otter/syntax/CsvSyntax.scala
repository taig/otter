package io.taig.otter.syntax

import io.taig.otter.Csv
import io.taig.otter.Field

/** Named wire contracts for Csv fields. Payload absence is independent of field absence. */
trait CsvSyntax:
  extension [S[-w, +r] <: Csv.Cell.Node[w, r], W, R](fa: Csv.Field.Schema[S, W, R])
    /** Requires the key and represents absence with an explicit blank. */
    def blank: Csv.Field.Schema[S, Option[W], Option[R]] = fa.optional(Field.Presence.Empty)

    /** Accepts missing or blank, and omits an absent field when writing. */
    def optionalOrBlank: Csv.Field.Schema[S, Option[W], Option[R]] = fa.optional(Field.Presence.OmittedOrEmpty)

    /** Accepts missing or blank, and writes an explicit blank for absence. */
    def blankOrMissing: Csv.Field.Schema[S, Option[W], Option[R]] = fa.optional(Field.Presence.EmptyOrMissing)

    def defaultedOnBlank(value: => R): Csv.Field.Schema[S, W, R] = fa.defaulted(value, Field.Absent.Empty)

    def defaultedOnMissingOrBlank(value: => R): Csv.Field.Schema[S, W, R] =
      fa.defaulted(value, Field.Absent.MissingOrEmpty)

object CsvSyntax extends CsvSyntax
