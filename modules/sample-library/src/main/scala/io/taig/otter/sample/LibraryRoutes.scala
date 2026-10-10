package io.taig.otter.sample

import cats.effect.Concurrent
import cats.effect.IO
import fs2.Stream
import io.taig.otter.Json
import io.taig.otter.http.Body
import io.taig.otter.http.Http4s
import io.taig.otter.http.Http4sCirce
import io.taig.otter.http.Http4sMultipart
import io.taig.otter.http.Route
import io.taig.otter.http.Routes
import io.taig.otter.http.codec.Http4sInterpreter
import io.taig.otter.sample.api.api
import io.taig.otter.sample.api.books
import io.taig.otter.sample.api.loans
import org.http4s.HttpApp

/** The endpoints of [[io.taig.otter.sample.api.api.served]], each paired with what answers it.
  *
  * The two lists are kept apart on purpose -- the declarations depend on nothing that serves them, so documents and
  * clients can be generated without a [[Library]] in scope -- and `LibraryServedTest` is what keeps them equal: the
  * declarations these routes carry must be exactly `api.served`, in order.
  *
  * A [[Route]] is an endpoint and an `A => F[B]`, and the pairing is checked by the compiler: the endpoint says what a
  * request holds and what an answer may be, so a handler of the wrong shape is a compile error rather than a runtime
  * surprise. Nothing here reaches into a request, builds a response, or writes down a status code.
  *
  * Routing asks a deliberately narrow question: the method, the number of path segments, and the literals among them. A
  * decode failure cannot tell "some other endpoint" from "this endpoint, called wrongly", and those need different
  * answers -- the first is the API's own `404`, or a `405` naming the methods where another route spells the path, and
  * the second stops here as a `400` -- so the decision is made on the part of a path that cannot vary.
  *
  * The consequence is worth knowing before adding a route: **a placeholder shadows a literal of the same arity**.
  * `/books/{isbn}` matches `/books/export` on arity and on its one literal, so whichever of the two is listed first
  * wins. The export is therefore registered first. The unserved CSV report still reaches the ISBN placeholder, as do
  * other methods on `/books/export` when their method selects that placeholder. `LibraryRoutesTest` records the
  * resulting statuses and `Allow` headers.
  */
object LibraryRoutes:
  type Payload = Body.Or[Json.Node, Http4sMultipart.Parts[Json.Node]]

  val payload =
    Http4sCirce.Payload.orElse(Http4sMultipart.payload(Http4sCirce.Payload)).withStreams(Http4sCirce.Streams)

  def routes(library: Library[IO]): Routes[IO, Http4sInterpreter.Supported[IO, LibraryRoutes.Payload, Json.Node]] =
    routes(library, api.default)

  def routes[F[_]](
      library: Library[F],
      definitions: api.Definitions[Stream[F, +*]]
  ): Routes[F, Http4sInterpreter.Supported[F, LibraryRoutes.Payload, Json.Node]] = Routes(
    Route(loans.health, (_: Unit) => library.health),
    Route(books.list, (filter, _) => library.list(filter)),
    Route(books.create, library.create),
    Route(definitions.streaming.exported, (_: Unit) => library.exported),
    Route(books.fetch, library.fetch),
    Route(books.patch, library.patch.tupled),
    Route(books.delete, library.delete),
    Route(books.scan, library.scan.tupled),
    Route(books.upload, library.upload.tupled),
    Route(books.intake, library.intake),
    Route(books.catalogue, (_: Unit) => library.catalogue),
    Route(loans.fetch, library.member),
    Route(loans.borrow, library.borrow.tupled)
  )

  def apply(library: Library[IO]): HttpApp[IO] = apply(library, api.default)

  def apply[F[_]: Concurrent](library: Library[F], definitions: api.Definitions[Stream[F, +*]]): HttpApp[F] =
    Http4s.app[F](definitions.all, LibraryRoutes.routes(library, definitions).values.toList*)(LibraryRoutes.payload)
