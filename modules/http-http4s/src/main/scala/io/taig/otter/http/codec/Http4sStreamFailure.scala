package io.taig.otter.http.codec

import io.taig.data.syntax.*
import io.taig.otter.Constraint
import io.taig.otter.Violations
import io.taig.otter.http.DecodingFailure
import io.taig.otter.http.Failure
import io.taig.validation.Violation

import scala.util.control.NoStackTrace

/** Internal element failure, contextualized at the request or response boundary. */
final private[http] case class Http4sStreamFailure(index: Long, failure: DecodingFailure)
    extends Exception("Stream element " + index, failure.cause.orNull)
    with NoStackTrace

private[http] object Http4sStreamFailure:
  def syntax(index: Long, reason: String): Http4sStreamFailure =
    apply(
      index,
      DecodingFailure(
        Failure.Category.Syntax,
        Violations(Violation(Constraint.Generic.Type(reason), "stream frame".asData, None))
      )
    )
  def encoding(index: Long, reason: String): Http4sStreamFailure =
    val failure = syntax(index, reason)
    failure.copy(failure = failure.failure.copy(category = Failure.Category.Encoding))
