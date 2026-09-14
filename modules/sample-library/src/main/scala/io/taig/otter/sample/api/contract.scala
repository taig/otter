package io.taig.otter.sample.api

import cats.syntax.all.*
import io.taig.otter.http.Api
import io.taig.otter.http.Code
import io.taig.otter.http.ErrorOverrides
import io.taig.otter.http.ErrorPolicy
import io.taig.otter.http.Failure
import io.taig.otter.http.Http4s
import io.taig.otter.http.Result
import io.taig.otter.sample.Problem
import io.taig.otter.sample.api.dsl.*

/** The same declared error responses are served, documented, and decoded by callers. */
object contract:
  def response(status: Int): Result.Schema[dsl.Payload, Failure, Problem] =
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

  val definition = Api(
    errors,
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
    loans.borrow,
    books.upload,
    books.exported,
    books.report
  ).flatMap(_.withErrors(books.catalogue, ErrorOverrides(unexpected = Some(response(503))))).toOption.get

  val health = definition.resolve(loans.health).toOption.get
  val listBooks = definition.resolve(books.list).toOption.get
  val createBook = definition.resolve(books.create).toOption.get
  val fetchBook = definition.resolve(books.fetch).toOption.get
  val patchBook = definition.resolve(books.patch).toOption.get
  val deleteBook = definition.resolve(books.delete).toOption.get
  val scanBooks = definition.resolve(books.scan).toOption.get
  val intakeBooks = definition.resolve(books.intake).toOption.get
  val catalogue = definition.resolve(books.catalogue).toOption.get
  val fetchLoans = definition.resolve(loans.fetch).toOption.get
  val borrow = definition.resolve(loans.borrow).toOption.get
