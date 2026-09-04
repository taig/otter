package io.taig.otter.http

import io.taig.otter.Absence
import io.taig.otter.Keys
import io.taig.otter.Metadata
import io.taig.otter.Tolerance

/** What every tier of the HTTP description agrees on.
  *
  * There is no single `Http.Schema` the way there is a single `Json.Schema`, because an endpoint is not one value on
  * one wire. A path is read out of a sequence of segments, a query string out of a sequence of name and value pairs, a
  * header set out of another, and a body out of bytes that a backend owns. Each of those is its own alphabet with its
  * own wire, declared in its own file, and this object holds only what they share.
  *
  * The tiers, from the outside in:
  *
  *   - [[Endpoint]] pairs a [[Request]] with a [[Response]].
  *   - [[Request]] is a [[Method]], a [[Path]], a [[Queries]], a [[Headers]] and at most one body; [[Response]] is a
  *     [[Results]], one [[Result]] per status [[Code]].
  *   - [[Bodies]] chooses between [[Body]] alternatives by media type, and a [[Body]] is a media type over a payload
  *     schema of some other alphabet entirely -- a JSON schema, a CSV schema -- or over bytes, or over [[Multipart]]
  *     parts.
  *   - [[Path]], [[Queries]] and [[Headers]] bottom out in [[Parameter]], the flat alphabet of what fits in a piece of
  *     text.
  */
object Http:
  /** The [[Metadata.Namespace]] the HTTP interpreters read their attributes from.
    *
    * An attribute set here wins over the same attribute set globally, so a schema can say one thing to every format and
    * another to HTTP alone.
    */
  val Namespace: Metadata.Namespace = Metadata.Namespace("http")

  /** The [[Absence]] a schema's metadata asks for. Asking for nothing is [[Absence.Omit]], because a query string and a
    * header set are both lists of what is there: a parameter with nothing to say is left out rather than sent empty.
    * This is where HTTP reads the shared vocabulary the same way round as JSON, and the opposite way from CSV, whose
    * columns are fixed by a header.
    */
  private[otter] def absence(metadata: Metadata): Absence =
    metadata.get(Http.Namespace, Metadata.Namespace.Global, Keys.absence).getOrElse(Absence.Omit)

  /** The [[Tolerance]] a schema's metadata asks for. Asking for nothing is [[Tolerance.Lenient]], so that a parameter
    * round trips whether its name is missing or merely present with nothing after the `=`.
    */
  private[otter] def tolerance(metadata: Metadata): Tolerance =
    metadata.get(Http.Namespace, Metadata.Namespace.Global, Keys.tolerance).getOrElse(Tolerance.Lenient)
