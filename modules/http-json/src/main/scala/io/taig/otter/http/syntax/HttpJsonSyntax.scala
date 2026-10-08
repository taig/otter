package io.taig.otter.http.syntax

import io.taig.otter.Json
import io.taig.otter.Reference
import io.taig.otter.http.Body
import io.taig.otter.http.Frame
import io.taig.otter.http.component.MediaTypeComponent

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
  ): Body.Schema[Body.Whole[Json.Schema[S, *, *]], W, R] =
    Body.Schema(Body.Value.Whole(MediaTypeComponent.json, Reference.later(schema)))

  def ndjson[C[+_]]: HttpJsonSyntax.Ndjson[C] = new HttpJsonSyntax.Ndjson[C]

object HttpJsonSyntax extends HttpJsonSyntax:
  final class Ndjson[C[+_]]:
    def apply[S[-w, +r] <: Json.Node[w, r], W, R](
        schema: => Json.Schema[S, W, R]
    ): Body.Schema[Body.Streamed.Requirement[C, Json.Schema[S, *, *]], C[W], C[R]] =
      Body.Schema(
        Body.Value
          .Streamed[C, Json.Schema[S, *, *], W, R](MediaTypeComponent.ndJson, Frame.Lines, Reference.later(schema))
      )
