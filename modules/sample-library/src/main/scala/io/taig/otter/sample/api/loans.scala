package io.taig.otter.sample.api

import io.taig.otter.http.Endpoint
import io.taig.otter.http.Path
import io.taig.otter.sample.Loan
import io.taig.otter.sample.Member
import io.taig.otter.sample.Problem
import io.taig.otter.sample.api.dsl.*

import java.util.UUID

/** What answers `POST /members/{reference}/loans`.
  *
  * The three branch case, and the one where a sum earns its keep: every branch carries a body, so every branch converts
  * -- and `:+` nests to the left, which means the raw shape is `Either[Either[Loan, Problem], Problem]` and the two
  * `Problem`s in it are told apart by nothing at all. A caller matching on that would be guessing. Matching on
  * [[Borrowed.Unknown]] against [[Borrowed.Unavailable]] is not guessing.
  */
enum Borrowed:
  case Lent(loan: Loan)
  case Unknown(problem: Problem)
  case Unavailable(problem: Problem)

/** Members, and the books they are holding. */
object loans:
  /** `/members/{reference}` */
  val one: Path[UUID] = __ / "members" / segment("reference", uuid)

  /** `GET /members/{reference}`, a member or no member -- see [[books.fetch]] for what `.to` is doing to the union. */
  val fetch: Endpoint[UUID, Option[Member]] = endpoint(
    request(method.get, loans.one),
    (result(code.ok)(body.json(schema.member)) :+ result(code.notFound)).to[Option[Member]]
  ).attr(openapi.operationId, "fetchMember")
    .attr(openapi.summary, "A member, their membership and what they owe")
    .attr(openapi.tags, "members")

  /** `POST /members/{reference}/loans`, answering three ways and carrying a document in each. */
  val borrow: Endpoint[(UUID, Loan.Request), Borrowed] = endpoint(
    request(method.post, loans.one / "loans")(body.json(schema.borrow)),
    (result(code.created)(body.json(schema.loan)).to[Borrowed.Lent] :+
      result(code.notFound)(body.json(schema.problem)).to[Borrowed.Unknown] :+
      result(code.conflict)(body.json(schema.problem)).to[Borrowed.Unavailable]).to[Borrowed]
  ).attr(openapi.operationId, "borrowBook")
    .attr(openapi.summary, "Lend a book to a member")
    .attr(openapi.tags, "loans")

  /** `GET /health`, which is the smallest endpoint there is: a literal path, nothing read, nothing written. */
  val health: Endpoint[Unit, Unit] = endpoint(
    request(method.get, __ / "health"),
    result(code.noContent)
  ).attr(openapi.operationId, "health").attr(openapi.tags, "service")
