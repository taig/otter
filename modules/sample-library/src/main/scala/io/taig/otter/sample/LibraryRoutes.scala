package io.taig.otter.sample

import cats.effect.Concurrent
import io.taig.otter.http.Http4s
import io.taig.otter.http.Http4sCirce
import io.taig.otter.http.Route
import io.taig.otter.sample.api.contract
import org.http4s.HttpRoutes

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
  /** Every served endpoint, answered by `library`. */
  def apply[F[_]: Concurrent](library: Library[F]): HttpRoutes[F] =
    Http4s.routes[F](Http4sCirce.Payload)(
      Route(contract.health, (_: Unit) => library.health),
      Route(contract.listBooks, input => library.list(input._1, input._2, input._3, input._4)),
      Route(contract.createBook, library.create),
      Route(contract.fetchBook, library.fetch),
      Route(contract.patchBook, library.patch.tupled),
      Route(contract.deleteBook, library.delete),
      Route(contract.scanBooks, library.scan.tupled),
      Route(contract.intakeBooks, library.intake),
      Route(contract.catalogue, (_: Unit) => library.catalogue),
      Route(contract.fetchLoans, library.member),
      Route(contract.borrow, library.borrow.tupled)
    )
