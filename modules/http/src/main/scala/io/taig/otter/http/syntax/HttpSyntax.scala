package io.taig.otter.http.syntax

import io.taig.otter.Field
import io.taig.otter.http.Body
import io.taig.otter.http.Header
import io.taig.otter.http.Http
import io.taig.otter.http.HttpKeys
import io.taig.otter.http.Parameter
import io.taig.otter.http.Part
import io.taig.otter.http.Query

/** Named absence contracts for HTTP's textual fields. */
trait HttpSyntax:
  extension [S[-w, +r] <: Parameter.Node[w, r], W, R](fa: Query.Schema[S, W, R])
    /** Requires the name and writes empty text for an absent value. */
    def empty: Query.Schema[S, Option[W], Option[R]] = fa.optional(Field.Presence.Empty)

    /** Accepts omission or empty text, and writes absence by omission. */
    def optionalOrEmpty: Query.Schema[S, Option[W], Option[R]] = fa.optional(Field.Presence.OmittedOrEmpty)

    /** Accepts omission or empty text, and writes absence as empty text. */
    def emptyOrMissing: Query.Schema[S, Option[W], Option[R]] = fa.optional(Field.Presence.EmptyOrMissing)

    def defaultedOnEmpty(value: => R): Query.Schema[S, W, R] = fa.defaulted(value, Field.Absent.Empty)

    def defaultedOnMissingOrEmpty(value: => R): Query.Schema[S, W, R] =
      fa.defaulted(value, Field.Absent.MissingOrEmpty)

  extension [S[-w, +r] <: Parameter.Node[w, r], W, R](fa: Header.Schema[S, W, R])
    /** Requires the name and writes empty text for an absent value. */
    def empty: Header.Schema[S, Option[W], Option[R]] = fa.optional(Field.Presence.Empty)

    /** Accepts omission or empty text, and writes absence by omission. */
    def optionalOrEmpty: Header.Schema[S, Option[W], Option[R]] = fa.optional(Field.Presence.OmittedOrEmpty)

    /** Accepts omission or empty text, and writes absence as empty text. */
    def emptyOrMissing: Header.Schema[S, Option[W], Option[R]] = fa.optional(Field.Presence.EmptyOrMissing)

    def defaultedOnEmpty(value: => R): Header.Schema[S, W, R] = fa.defaulted(value, Field.Absent.Empty)

    def defaultedOnMissingOrEmpty(value: => R): Header.Schema[S, W, R] =
      fa.defaulted(value, Field.Absent.MissingOrEmpty)

  extension [B[-w, +r] <: Body.Node[w, r], W, R](fa: Part.Schema[B, W, R])
    /** The name this part claims the bytes it carries were saved under. */
    def filename(value: String): Part.Schema[B, W, R] = fa.attr(Http.Namespace, HttpKeys.filename, value)

object HttpSyntax extends HttpSyntax
