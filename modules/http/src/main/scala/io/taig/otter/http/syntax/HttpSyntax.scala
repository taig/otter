package io.taig.otter.http.syntax

import io.taig.otter.Absence
import io.taig.otter.Keys
import io.taig.otter.Tolerance
import io.taig.otter.http.Body
import io.taig.otter.http.Header
import io.taig.otter.http.Http
import io.taig.otter.http.HttpKeys
import io.taig.otter.http.Parameter
import io.taig.otter.http.Part
import io.taig.otter.http.Query

/** What a parameter says about being absent.
  *
  * Written against a query parameter and a header rather than against a [[Parameter]], because being absent is
  * something only a named member of a set can do: a value has nowhere to be absent from. The two positions get their
  * own extension rather than one over [[io.taig.otter.Annotated]], for the reason `JsonSyntax` gives -- a path segment
  * is annotated too, and a segment that may not be there is a different path.
  */
trait HttpSyntax:
  extension [S[-w, +r] <: Parameter.Node[w, r], W, R](fa: Query.Schema[S, W, R])
    /** How the parameter is written when what it holds is absent. Inert on one that is always there. */
    def absence(value: Absence): Query.Schema[S, W, R] = fa.attr(Http.Namespace, Keys.absence, value)

    /** Gives the name with nothing after the `=` when the value is absent. */
    def emptied: Query.Schema[S, W, R] = absence(Absence.Empty)

    /** Leaves the name out when the value is absent, which is what a parameter does anyway. */
    def omitted: Query.Schema[S, W, R] = absence(Absence.Omit)

    /** Whether the parameter accepts only the form [[absence]] names, or either of them. */
    def tolerance(value: Tolerance): Query.Schema[S, W, R] = fa.attr(Http.Namespace, Keys.tolerance, value)

    /** Rejects the form [[absence]] does not name: an empty value for an omitted parameter, a missing name for an
      * emptied one.
      *
      * This is also what a flag needs. `?verbose` and `?verbose=` are both a name carrying no text, and a lenient
      * parameter reads that as absence before the value is ever looked at -- which is right for `?page=` and wrong for
      * a boolean, where giving the name at all is the assertion. Saying `strict` is what lets the empty text through to
      * [[io.taig.otter.http.codec.ParameterCoerceDecoder]], which reads it as `true`.
      */
    def strict: Query.Schema[S, W, R] = tolerance(Tolerance.Strict)

    /** Reads a missing name and an empty value alike, which is what a parameter does anyway. */
    def lenient: Query.Schema[S, W, R] = tolerance(Tolerance.Lenient)

  extension [S[-w, +r] <: Parameter.Node[w, r], W, R](fa: Header.Schema[S, W, R])
    /** How the header is written when what it holds is absent. Inert on one that is always there. */
    def absence(value: Absence): Header.Schema[S, W, R] = fa.attr(Http.Namespace, Keys.absence, value)

    /** Sends the name with nothing after the colon when the value is absent. */
    def emptied: Header.Schema[S, W, R] = absence(Absence.Empty)

    /** Leaves the name out when the value is absent, which is what a header does anyway. */
    def omitted: Header.Schema[S, W, R] = absence(Absence.Omit)

    /** Whether the header accepts only the form [[absence]] names, or either of them. */
    def tolerance(value: Tolerance): Header.Schema[S, W, R] = fa.attr(Http.Namespace, Keys.tolerance, value)

    /** Rejects the form [[absence]] does not name. */
    def strict: Header.Schema[S, W, R] = tolerance(Tolerance.Strict)

    /** Reads a missing name and an empty value alike, which is what a header does anyway. */
    def lenient: Header.Schema[S, W, R] = tolerance(Tolerance.Lenient)

  extension [B[-w, +r] <: Body.Node[w, r], W, R](fa: Part.Schema[B, W, R])
    /** The name this part claims the bytes it carries were saved under. */
    def filename(value: String): Part.Schema[B, W, R] = fa.attr(Http.Namespace, HttpKeys.filename, value)

object HttpSyntax extends HttpSyntax
