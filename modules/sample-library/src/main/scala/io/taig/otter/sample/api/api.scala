package io.taig.otter.sample.api

import cats.data.Chain
import io.taig.otter.http.Endpoint
import io.taig.otter.http.OpenApi

/** Every endpoint this API describes, in two lists.
  *
  * The split is the honest one and not a tidying: [[api.served]] is what the http4s interpreter carries, and
  * [[api.unserved]] is what it does not. Both are rendered into the OpenAPI document and both are offered to the
  * TypeScript generator, because a description is a description whether or not this repository happens to have an
  * interpreter for it -- and each renderer reports what it could not carry rather than quietly dropping it.
  */
object api:
  val Info: OpenApi.Info = OpenApi.Info(
    title = "Otter Library",
    version = "1.0.0",
    description = Some("A library management API, written to show what Otter can describe")
  )

  /** The endpoints [[io.taig.otter.sample.LibraryRoutes]] answers. */
  val served: Chain[Endpoint.Node] = Chain(
    loans.health,
    books.list,
    books.create,
    books.fetch,
    books.patch,
    books.delete,
    books.scan,
    books.intake,
    books.catalogue,
    loans.fetch,
    loans.borrow
  )

  /** The endpoints nothing here answers, and why each one cannot be.
    *
    *   - [[books.upload]] carries a [[io.taig.otter.http.Multipart]] payload, which the http4s interpreter reports as
    *     `Http4sIssue.Uninterpreted` and the TypeScript renderer as `TypescriptIssue.Multipart`.
    *   - [[books.exported]] answers with a streamed body. What a sequence of elements is belongs to whoever has an
    *     effect type to say it in, and `otter-http` deliberately has none, so every interpreter here reports it.
    *   - [[books.report]] answers with a stream whose elements are written in the CSV alphabet, which is a payload
    *     nothing in this repository interprets at all.
    *
    * Two of them are additionally shadowed: `/books/export` and `/books/report` have the arity and the leading literal
    * of `/books/{isbn}`, so a request for either reaches `books.fetch` and is answered `400` for an ISBN that does not
    * parse rather than `404`. Were they ever served, they would have to be registered ahead of it.
    *
    * They are here rather than deleted because being told what cannot be carried is the feature. A renderer always
    * returns a document and a list of issues; it never throws and never half emits, and `LibraryShortfallTest` holds it
    * to that.
    */
  val unserved: Chain[Endpoint.Node] = Chain(books.upload, books.exported, books.report)

  /** Everything, for the renderers. */
  val all: Chain[Endpoint.Node] = api.served ++ api.unserved
