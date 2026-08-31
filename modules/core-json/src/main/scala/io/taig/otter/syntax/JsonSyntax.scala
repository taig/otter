package io.taig.otter.syntax

import io.taig.otter.Absence
import io.taig.otter.Json
import io.taig.otter.Keys
import io.taig.otter.Tolerance

trait JsonSyntax:
  /** These are written against a field rather than a schema, because omitting is something only a record's member can
    * do: a schema has nowhere to be absent from.
    */
  extension [S[-w, +r] <: Json.Node[w, r], W, R](fa: Json.Field.Schema[S, W, R])
    /** How the field renders when what it holds is absent. Inert on a field that is always there. */
    def absence(value: Absence): Json.Field.Schema[S, W, R] = fa.attr(Json.Namespace, Keys.absence, value)

    /** Writes the key with an explicit `null` when the value is absent. */
    def nullable: Json.Field.Schema[S, W, R] = absence(Absence.Empty)

    /** Drops the key when the value is absent, which is what a field does anyway. */
    def omitted: Json.Field.Schema[S, W, R] = absence(Absence.Omit)

    /** Whether the field accepts only the form [[absence]] names, or either of them. */
    def tolerance(value: Tolerance): Json.Field.Schema[S, W, R] = fa.attr(Json.Namespace, Keys.tolerance, value)

    /** Rejects the form [[absence]] does not name: a `null` for an omitted field, a missing key for a nullable one. */
    def strict: Json.Field.Schema[S, W, R] = tolerance(Tolerance.Strict)

    /** Reads a missing key and an explicit `null` alike, which is what a field does anyway. */
    def lenient: Json.Field.Schema[S, W, R] = tolerance(Tolerance.Lenient)

object JsonSyntax extends JsonSyntax
