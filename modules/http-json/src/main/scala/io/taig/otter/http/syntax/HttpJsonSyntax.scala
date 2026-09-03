package io.taig.otter.http.syntax

import io.taig.otter.Annotation
import io.taig.otter.Json
import io.taig.otter.Reference
import io.taig.otter.http.Body
import io.taig.otter.http.Frame
import io.taig.otter.http.MediaType

/** Bodies carrying JSON.
  *
  * A module of its own rather than a few lines in `otter-http`, so that describing an endpoint does not drag in a JSON
  * alphabet. A body's payload is any schema at all, and this is what says one of them may be a JSON one -- the same
  * shape a second payload alphabet takes, whether that is CSV, a form encoding, or something a downstream project
  * defines.
  *
  * The payload type is kept at the `S` the schema was built with rather than widened to [[Json.Node]], so a body
  * carrying a flat record still says so and a renderer that only accepts flat records can still refuse the rest.
  */
trait HttpJsonSyntax:
  /** A body carrying one JSON document. */
  def json[S[-w, +r] <: Json.Node[w, r], W, R](
      schema: => Json.Schema[S, W, R]
  ): Body.Schema[[w, r] =>> Json.Schema[S, w, r], W, R] =
    Body.Schema(Body.Value.Whole(MediaType.Json, Reference.later(schema)))

  /** A body carrying JSON documents one per line, which is what `application/x-ndjson` is.
    *
    * The result carries the element type, so a backend handed this body knows what its stream yields; `.body` is the
    * same body as a request holds it, which is as something contributing nothing to what the request reads.
    */
  def ndjson[S[-w, +r] <: Json.Node[w, r], W, R](
      schema: => Json.Schema[S, W, R]
  ): Body.Streamed.Schema[[w, r] =>> Json.Schema[S, w, r], W, R] =
    new Body.Streamed.Schema(Annotation(Body.Value.Streamed(MediaType.NdJson, Frame.Lines, Reference.later(schema))))

object HttpJsonSyntax extends HttpJsonSyntax
