package io.taig.otter.sample.api

import cats.syntax.all.*
import io.taig.otter.http.ErrorPolicy
import io.taig.otter.http.Failure
import io.taig.otter.http.Http4s
import io.taig.otter.http.Response
import io.taig.otter.http.Status
import io.taig.otter.http.Unrouted
import io.taig.otter.http.UnroutedPolicy
import io.taig.otter.sample.Problem
import io.taig.otter.sample.api.dsl.*

/** The same declared error responses are served, documented, and decoded by callers.
  *
  * The unrouted answers are served in the same [[Problem]] shape and are neither documented nor decoded, because no
  * endpoint owns them: a caller meets one only when it and the server disagree about what exists. One consequence is
  * worth knowing. [[io.taig.otter.sample.api.loans.borrow]] declares a `404` of its own carrying a [[Problem]], so a
  * caller out of step with the server reads an unrouted `404` as `Borrowed.Unknown` -- which is why its kind is
  * [[Problem.Unrouted]] and not [[Problem.Missing]].
  */
object contract:
  def answer(status: Int): Response.Schema[dsl.Payload, Failure, Problem] =
    response(Status(status))(body.json(schema.problem)).dimap[Failure, Problem](failure =>
      failure.category match
        case Failure.Category.Envelope | Failure.Category.Syntax | Failure.Category.ContentType |
            Failure.Category.Validation =>
          Problem.malformed(failure.violations.fold(List.empty[String])(Http4s.report(_).linesIterator.toList))
        case Failure.Category.EntityRead | Failure.Category.Encoding | Failure.Category.Status |
            Failure.Category.Unexpected =>
          Problem.internal
    )(identity)

  val errors: ErrorPolicy[dsl.Payload, Problem] = errorPolicy.from(answer(500))(
    envelope = answer(400),
    syntax = answer(400),
    contentType = answer(415),
    validation = answer(422)
  )

  val unrouted: UnroutedPolicy[dsl.Payload] = unroutedPolicy(
    notFound =
      response(status.notFound)(body.json(schema.problem)).dimap[Unrouted.NotFound, Problem](_ => Problem.notFound)(
        identity
      ),
    methodNotAllowed =
      response(status.methodNotAllowed)(body.json(schema.problem)).dimap[Unrouted.MethodNotAllowed, Problem](unrouted =>
        Problem.methodNotAllowed(unrouted.allowed.toChain.toList.map(_.name))
      )(identity)
  )
