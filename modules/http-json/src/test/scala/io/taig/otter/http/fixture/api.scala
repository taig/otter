package io.taig.otter.http.fixture

import io.taig.otter.Json
import io.taig.otter.Keys
import io.taig.otter.component.JsonComponent
import io.taig.otter.http.Bodies
import io.taig.otter.http.Body
import io.taig.otter.http.Endpoint
import io.taig.otter.http.MediaType
import io.taig.otter.http.Multipart
import io.taig.otter.http.Path
import io.taig.otter.http.Queries
import io.taig.otter.http.component.BodyComponent
import io.taig.otter.http.component.HttpComponent
import io.taig.otter.http.fixture.dsl.*
import io.taig.otter.http.syntax.HttpJsonSyntax
import scodec.bits.ByteVector

/** Two vocabularies, named apart.
  *
  * They collide on every primitive -- both have a `string` -- and that is not an accident to work around but the
  * distinction itself: a piece of a URL and a JSON string are not the same thing, and a position that takes one does
  * not take the other. Naming one and importing the other is how the codebase already recommends mixing vocabularies.
  *
  * `body` is overridden rather than inherited as-is, so that `body.json` and `body.ndjson` sit next to `body.binary`
  * and `body.multipart` -- one namespace for every shape a body comes in, JSON included.
  */
object dsl extends HttpComponent:
  override val body: BodyComponent & HttpJsonSyntax = new BodyComponent with HttpJsonSyntax {}

object payload extends JsonComponent

final case class Report(title: String, pages: Int)

final case class Upload(report: Report, attachment: ByteVector)

final case class Settings(theme: String)

final case class Tree(value: Int, children: List[Tree])

object api:
  /** Kept at its record type rather than widened to `Json[Report]`, because `Annotated` lives on the node wrappers and
    * not on the abstract schema they share -- so a widened schema can no longer be given a name.
    */
  val report: Json.Record[Report] =
    (payload.field("title", payload.string) :* payload.field("pages", payload.int)).to

  /** One document. */
  val reported: Body.Of[Json.Node, Report] = body.json(api.report)

  /** A body that may be either of two things, which is what content negotiation describes. The alternatives are written
    * in different alphabets -- one a JSON schema, the other no schema at all -- and the union of their payload types is
    * what the body's own type records.
    */
  val negotiated: Bodies[Either[Report, ByteVector]] = body.json(api.report) :+ body.binary(MediaType.Pdf)

  /** A multipart upload: a JSON part and a file part, which is the shape neither earlier attempt could write down.
    *
    * The file part carries a `filename`, and its body carries its own media type -- both of which had nowhere to live
    * in a flat form alphabet whose only leaf was a string.
    */
  val upload: Multipart[Upload] =
    (part("report", body.json(api.report)) :*
      part("attachment", body.binary(MediaType.Pdf)).filename("report.pdf")).to

  /** The same upload as a body, which is all a multipart body is: a body whose payload happens to be a set of parts. */
  val uploaded: Body.Of[Multipart.Node, Upload] = body.multipart(api.upload)

  /** A stream of documents, one per line. The element type is on the body, so a backend handed it knows what its stream
    * yields; the body itself contributes nothing to what a request reads.
    */
  val reports: Body.Streamed.Of[Json.Node, Report] = body.ndjson(api.report)

  /** `/reports/{id}` */
  val one: Path[Int] = __ :* segment("reports") :* segment("id", int)

  /** `?page`, defaulted, so a caller that says nothing still gets an answer. */
  val paging: Queries[Int] = query("page", int).optional(1).toRecord

  /** `GET /reports/{id}?page`, answering with a report or saying there is none.
    *
    * Read as a server sees it: the request is what this endpoint *reads* and the responses what it *writes*, which is
    * the side a document written for its callers has to describe.
    */
  val fetch: Endpoint.Server[Body.Payload, (Int, Int), Either[Report, Unit]] =
    endpoint(
      request(method.get, api.one).queries(api.paging),
      result(code.ok)(body.json(api.report)) :+ result(code.notFound)
    )

  /** `POST /reports` taking a multipart upload and answering with the report it made. */
  val create: Endpoint.Server[Body.Payload, Upload, Report] =
    endpoint(
      request(method.post, __ :* segment("reports"))(api.uploaded),
      result(code.created)(body.json(api.report)).toUnion
    )

  /** `GET /reports` answering with a stream of reports, which contributes nothing to what the caller is handed here.
    */
  val stream: Endpoint.Server[Body.Payload, Unit, Unit] =
    endpoint(
      request(method.get, __ :* segment("reports")),
      result(code.ok)(api.reports).toUnion
    )

  /** The same report, named, so a document declares it once under `components/schemas` and refers to it from everywhere
    * it is used.
    */
  val named: Json.Record[Report] = api.report.attr(Keys.name, "Report")

  /** An upload whose attachment need not be sent, to show that a part is a field and carries a field's optionality. */
  val partial: Multipart[(Report, Option[ByteVector])] =
    part("report", body.json(api.named)) :*
      part("attachment", body.binary(MediaType.Pdf)).filename("report.pdf").optional

  /** `PUT /reports/{id}` taking the partial upload and answering with the named report. */
  val replace: Endpoint.Server[Body.Payload, (Int, (Report, Option[ByteVector])), Report] =
    endpoint(
      request(method.put, api.one)(body.multipart(api.partial)),
      result(code.ok)(body.json(api.named)).toUnion
    )

  /** A payload with a defaulted field, which is the case where the two sides of a schema genuinely differ: a reader
    * accepts its absence and a writer always produces it.
    */
  val settings: Json.Record[Settings] = payload.field("theme", payload.string).optional("dark").toRecord.to

  /** `PUT /settings`, to be rendered from both sides and compared. */
  val configure: Endpoint.Server[Body.Payload, Settings, Unit] =
    endpoint(
      request(method.put, __ :* segment("settings"))(body.json(api.settings)),
      result(code.noContent).toUnion
    )

  /** `PATCH /settings`, whose body need not be sent at all.
    *
    * The absence reaches the handler as one, which is what an `Option` on the request's own value says: a body that was
    * not sent is not a body that held the schema's defaults.
    */
  val amend: Endpoint.Server[Body.Payload, Option[Settings], Unit] =
    endpoint(
      request(method.patch, __ :* segment("settings"))(body.optional(body.json(api.settings))),
      result(code.noContent).toUnion
    )

  /** A payload that refers to itself, which only works because it is named: a definition is what a `$ref` points at. */
  lazy val tree: Json.Record[Tree] =
    (payload.field("value", payload.int) :* payload.field("children", payload.collection.list(api.tree)))
      .to[Tree]
      .attr(Keys.name, "Tree")

  /** `GET /trees` answering with one. */
  val trees: Endpoint.Server[Body.Payload, Unit, Tree] =
    endpoint(request(method.get, __ :* segment("trees")), result(code.ok)(body.json(api.tree)).toUnion)
