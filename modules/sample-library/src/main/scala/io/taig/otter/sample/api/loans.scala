package io.taig.otter.sample.api

import io.taig.otter.http.Code
import io.taig.otter.http.Endpoint
import io.taig.otter.http.Method
import io.taig.otter.http.OpenApiKeys
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

  /** `GET /members/{reference}` */
  val fetch: Endpoint[UUID, Either[Member, Unit]] = endpoint(
    request(Method.Get, loans.one),
    result(Code.Ok).body(body.json(schema.member)) :+ result(Code.NotFound)
  ).attr(OpenApiKeys.operationId, "fetchMember")
    .attr(OpenApiKeys.summary, "A member, their membership and what they owe")
    .attr(OpenApiKeys.tags, "members")

  /** `POST /members/{reference}/loans`, answering three ways and carrying a document in each. */
  val borrow: Endpoint[(UUID, Loan.Request), Borrowed] = endpoint(
    request(Method.Post, loans.one / "loans").body(body.json(schema.borrow)),
    (result(Code.Created).body(body.json(schema.loan)).to[Borrowed.Lent] :+
      result(Code.NotFound).body(body.json(schema.problem)).to[Borrowed.Unknown] :+
      result(Code.Conflict).body(body.json(schema.problem)).to[Borrowed.Unavailable]).to[Borrowed]
  ).attr(OpenApiKeys.operationId, "borrowBook")
    .attr(OpenApiKeys.summary, "Lend a book to a member")
    .attr(OpenApiKeys.tags, "loans")

  /** `GET /health`, which is the smallest endpoint there is: a literal path, nothing read, nothing written. */
  val health: Endpoint[Unit, Unit] = endpoint(
    request(Method.Get, __ / "health"),
    result(Code.NoContent).toUnion
  ).attr(OpenApiKeys.operationId, "health").attr(OpenApiKeys.tags, "service")
