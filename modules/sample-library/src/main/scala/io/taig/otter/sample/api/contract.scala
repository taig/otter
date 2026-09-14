package io.taig.otter.sample.api

import cats.syntax.all.*
import io.taig.otter.http.Api
import io.taig.otter.http.ApiIssue
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
  @SuppressWarnings(Array("scalafix:DisableSyntax.throw"))
  def checked[A](result: Either[ApiIssue, A]): A = result match
    case Right(value) => value
    case Left(issue)  => throw new IllegalStateException(s"Invalid sample API declaration: $issue")

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

  val definition = checked:
    Api(
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
    ).flatMap(_.withErrors(books.catalogue, ErrorOverrides(unexpected = Some(response(503)))))

  val health = checked(definition.resolve(loans.health))
  val listBooks = checked(definition.resolve(books.list))
  val createBook = checked(definition.resolve(books.create))
  val fetchBook = checked(definition.resolve(books.fetch))
  val patchBook = checked(definition.resolve(books.patch))
  val deleteBook = checked(definition.resolve(books.delete))
  val scanBooks = checked(definition.resolve(books.scan))
  val intakeBooks = checked(definition.resolve(books.intake))
  val catalogue = checked(definition.resolve(books.catalogue))
  val fetchLoans = checked(definition.resolve(loans.fetch))
  val borrow = checked(definition.resolve(loans.borrow))
