package io.taig.otter.sample.api

import cats.syntax.all.*
import io.taig.otter.http.ErrorPolicy
import io.taig.otter.http.Failure
import io.taig.otter.http.Http4s
import io.taig.otter.http.Response
import io.taig.otter.http.Status
import io.taig.otter.sample.Problem
import io.taig.otter.sample.api.dsl.*

/** The same declared error responses are served, documented, and decoded by callers. */
object contract:
  def answer(status: Int): Response.Schema[dsl.Payload, Failure, Problem] =
    response(Status(status))(body.json(schema.problem)).dimap[Failure, Problem](failure =>
      if status == 500 then Problem.internal
      else Problem.malformed(failure.violations.fold(List.empty[String])(Http4s.report(_).linesIterator.toList))
    )(identity)

  val errors: ErrorPolicy[dsl.Payload, Problem] = ErrorPolicy(
    answer(400),
    answer(400),
    answer(415),
    answer(422),
    answer(500),
    answer(500),
    answer(500),
    answer(500),
    answer(500)
  )
