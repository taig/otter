package io.taig.otter.sample

import cats.data.Chain
import cats.effect.Concurrent
import io.taig.otter.Violations
import io.taig.otter.codec.JsonCirceEncoder
import io.taig.otter.http.Http4s
import io.taig.otter.http.Http4sCirce
import io.taig.otter.http.Http4sWire
import io.taig.otter.http.Route
import io.taig.otter.sample.api.books
import io.taig.otter.sample.api.dsl
import io.taig.otter.sample.api.loans
import io.taig.otter.sample.api.schema
import org.http4s.HttpRoutes
import scodec.bits.ByteVector

/** The endpoints of [[io.taig.otter.sample.api.api.served]], each paired with what answers it.
  *
  * A [[Route]] is an endpoint and an `A => F[B]`, and the pairing is checked by the compiler: the endpoint says what a
  * request holds and what an answer may be, so a handler of the wrong shape is a compile error rather than a runtime
  * surprise. Nothing here reaches into a request, builds a response, or writes down a status code.
  *
  * Routing asks a deliberately narrow question: the method, the number of path segments, and the literals among them. A
  * decode failure cannot tell "some other endpoint" from "this endpoint, called wrongly", and those need different
  * answers -- the first falls through to a `404`, the second stops here as a `400` -- so the decision is made on the
  * part of a path that cannot vary.
  *
  * The consequence is worth knowing before adding a route: **a placeholder shadows a literal of the same arity**.
  * `/books/{isbn}` matches `/books/export` on arity and on its one literal, so whichever of the two is listed first
  * wins, and a literal path must be registered before the placeholder path it would otherwise be swallowed by. Neither
  * `/books/export` nor `/books/report` is served here -- see [[io.taig.otter.sample.api.api.unserved]] -- so both are
  * caught by `books.fetch` and answered `400` for an ISBN that does not parse, which is what `LibraryRoutesTest`
  * records rather than leaves to be discovered.
  */
object LibraryRoutes:
  /** What a request that this API described but did not hold is answered with.
    *
    * [[Http4s.routes]] takes this as a parameter precisely so an API's errors stay its own vocabulary, and passing one
    * is the difference between a caller reading plain text from the framework and reading the same [[Problem]] document
    * it gets from every handler. The 400/422 split is [[Http4s.code]]'s and is kept: violations found only under the
    * body mean the content was understood and wrong, and anything in the envelope means the request was.
    */
  val malformed: Violations => Http4sWire.Response = violations =>
    val problem = Problem.malformed(Http4s.report(violations).linesIterator.toList)
    val document = JsonCirceEncoder.encode(schema.problem, problem).noSpaces
    val bytes = ByteVector.encodeUtf8(document).getOrElse(ByteVector.empty)

    Http4sWire.Response(Http4s.code(violations), Chain.empty, Some((dsl.mediaType.json, bytes)))

  /** Every served endpoint, answered by `library`. */
  def apply[F[_]: Concurrent](library: Library[F]): HttpRoutes[F] =
    Http4s.routes[F](Http4sCirce.Payload, LibraryRoutes.malformed)(
      Route(loans.health, (_: Unit) => library.health),
      Route(books.list, (page, size, genres, available, _) => library.list(page, size, genres, available)),
      Route(books.create, library.create),
      Route(books.fetch, library.fetch),
      Route(books.patch, library.patch.tupled),
      Route(books.delete, library.delete),
      Route(books.scan, library.scan.tupled),
      Route(books.intake, library.intake),
      Route(books.catalogue, (_: Unit) => library.catalogue),
      Route(loans.fetch, library.member),
      Route(loans.borrow, library.borrow.tupled)
    )
