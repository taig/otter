package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Violations

import scala.util.control.NoStackTrace

/** What can go wrong that is not a violation of a schema.
  *
  * An exception rather than a value, because these reach a caller through `F` and there is nothing to accumulate: a
  * response that does not answer the endpoint is not a partially usable one. [[Http4sFailure.Response]] carries the
  * `Violations` whole rather than rendering them, so a caller may report them however it already reports the ones a
  * request produced -- which is the divergence [[io.taig.otter.JsonBorer]] documents in the other direction, where
  * borer's error type could not hold a path and one had to be rendered into the message.
  */
sealed abstract class Http4sFailure(message: String) extends Exception(message) with NoStackTrace

object Http4sFailure:
  /** The response did not hold what the endpoint said it would. */
  final case class Response(violations: Violations)
      extends Http4sFailure(show"Response does not match the endpoint:\n${Http4s.report(violations)}")

  /** Something the endpoint describes that this interpreter cannot carry. */
  final case class Interpreter(issue: Http4sIssue) extends Http4sFailure(issue.show)

  /** A method http4s will not send. */
  final case class Method(method: io.taig.otter.http.Method, reason: String)
      extends Http4sFailure(show"Cannot send method '${method.name}': $reason")

  /** A status code http4s will not answer with. */
  final case class Code(code: io.taig.otter.http.Code, reason: String)
      extends Http4sFailure(show"Cannot answer with code '${code.value}': $reason")
