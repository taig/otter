package io.taig.otter.syntax

import io.taig.otter.Field
import io.taig.otter.Json
import io.taig.otter.Optional
import io.taig.otter.Reference

/** Named wire contracts for Json fields. Payload absence is independent of field absence. */
trait JsonSyntax:
  extension [S[-w, +r] <: Json.Node[w, r], W, R](schema: S[W, R])
    /** A present JSON value that may be null. This never makes a field's key optional. */
    def nullable: Json.Optional.Schema[S, Option[W], Option[R]] =
      Json.Optional.Schema(Optional.Root(Reference.now(schema)))

  extension [S[-w, +r] <: Json.Node[w, r], W, R](fa: Json.Field.Schema[S, W, R])
    /** Requires the key and represents absence with an explicit null. */
    def nullable: Json.Field.Schema[S, Option[W], Option[R]] = fa.optional(Field.Presence.Empty)

    /** Accepts missing or null, and omits an absent field when writing. */
    def optionalOrNull: Json.Field.Schema[S, Option[W], Option[R]] = fa.optional(Field.Presence.OmittedOrEmpty)

    /** Accepts missing or null, and writes an explicit null for absence. */
    def nullableOrMissing: Json.Field.Schema[S, Option[W], Option[R]] = fa.optional(Field.Presence.EmptyOrMissing)

    def defaultedOnNull(value: => R): Json.Field.Schema[S, W, R] = fa.defaulted(value, Field.Absent.Empty)

    def defaultedOnMissingOrNull(value: => R): Json.Field.Schema[S, W, R] =
      fa.defaulted(value, Field.Absent.MissingOrEmpty)

object JsonSyntax extends JsonSyntax
