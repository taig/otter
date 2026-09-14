package io.taig.otter.sample.api

import cats.syntax.all.*
import io.taig.otter.http.Code
import io.taig.otter.http.ErrorPolicy
import io.taig.otter.http.Failure
import io.taig.otter.http.Http4s
import io.taig.otter.http.Result
import io.taig.otter.sample.Problem
import io.taig.otter.sample.api.dsl.*

/** The same declared error responses are served, documented, and decoded by callers. */
object contract:
  private def response(status: Int): Result.Schema[dsl.Payload, Failure, Problem] =
    result(Code(status))(body.json(schema.problem)).dimap[Failure, Problem](failure =>
      if status == 500 then Problem.internal
      else Problem.malformed(failure.violations.fold(List.empty[String])(Http4s.report(_).linesIterator.toList))
    )(identity)

  val errors: ErrorPolicy[dsl.Payload, Problem] = ErrorPolicy(
    response(400),
    response(400),
    response(415),
    response(422),
    response(500),
    response(500),
    response(500),
    response(500),
    response(500)
  )

  val health = errors(loans.health)
  val listBooks = errors(books.list)
  val createBook = errors(books.create)
  val fetchBook = errors(books.fetch)
  val patchBook = errors(books.patch)
  val deleteBook = errors(books.delete)
  val scanBooks = errors(books.scan)
  val intakeBooks = errors(books.intake)
  val catalogue = errors(books.catalogue)
  val fetchLoans = errors(loans.fetch)
  val borrow = errors(loans.borrow)
