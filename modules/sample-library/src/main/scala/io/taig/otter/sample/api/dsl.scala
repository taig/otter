package io.taig.otter.sample.api

import io.taig.otter.Csv
import io.taig.otter.Json
import io.taig.otter.component.CaseInsensitiveComponent
import io.taig.otter.component.CsvComponent
import io.taig.otter.component.IronComponent
import io.taig.otter.component.JavaTimeComponent
import io.taig.otter.component.JsonComponent
import io.taig.otter.http.component.BodyComponent
import io.taig.otter.http.component.HttpComponent
import io.taig.otter.http.syntax.HttpJsonSyntax

/** The envelope vocabulary: what a path, a query string and a header set are made of.
  *
  * `body` is overridden rather than inherited as-is, so that `body.json` and `body.ndjson` sit next to `body.binary`
  * and `body.multipart` -- one namespace for every shape a body comes in, JSON included. That also keeps the bare name
  * `json` free for the payload vocabulary below.
  */
object dsl extends HttpComponent:
  override val body: BodyComponent & HttpJsonSyntax = new BodyComponent with HttpJsonSyntax {}

/** The payload vocabulary: what a document is made of.
  *
  * Two objects rather than one, and named apart rather than merged, because they collide on every primitive and the
  * collision is the distinction itself: `dsl.string` is a piece of a URL and `json.string` is a JSON string, and a
  * position that takes one does not take the other. That is the idiom the http fixtures already recommend, and it is
  * why an endpoint here reads `segment("isbn", isbn)` next to `body.json(schema.book)` with no ambiguity.
  *
  * Three optional component modules are mixed in beside the JSON one, each contributing a vocabulary rather than a
  * format: `java.time` values carried as text, a case insensitive string, and -- in a namespace of its own, so the
  * refined and unrefined member of a name never meet in overload resolution -- iron refinements whose type carries the
  * constraint their validation checks.
  */
object json
    extends JsonComponent,
      JavaTimeComponent[Json.Primitive.Text.Schema],
      CaseInsensitiveComponent[Json.Primitive.Text.Schema]:
  object refined
      extends IronComponent.Number[Json.Primitive.Number.Schema],
        IronComponent.Text[Json.Primitive.Text.Schema],
        IronComponent.Collection[Json.Node, Json.Collection.Schema]

/** The row vocabulary, for the one body written in an alphabet nothing here interprets.
  *
  * A CSV row is a record whose members are all cells, which is a different alphabet from JSON rather than a subset of
  * it -- there is no `branch`, no `collection` and no `dictionary`, because none of those fit in a cell.
  */
object csv extends CsvComponent, JavaTimeComponent[Csv.Primitive.Text.Schema]
