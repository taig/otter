package io.taig.otter.sample.api

import cats.data.Chain
import io.taig.otter.http.Api
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

  final class Definitions[C[+_]]:
    val streaming = new books.Streaming[C]

    val served: Chain[Endpoint.Declaration.Node] = Chain(
      loans.health,
      books.list,
      books.create,
      streaming.exported,
      books.fetch,
      books.patch,
      books.delete,
      books.scan,
      books.upload,
      books.intake,
      books.catalogue,
      loans.fetch,
      loans.borrow
    )

    val unserved: Chain[Endpoint.Declaration.Node] = Chain(streaming.report)

    val all = Api(served ++ unserved, contract.errors, contract.unrouted)

  val default: api.Definitions[fs2.Stream[cats.effect.IO, +*]] = new api.Definitions
  export default.{all, served, unserved}
