package io.taig.otter.http

import io.taig.otter.Violations
import cats.syntax.all.*

sealed abstract class HttpError extends Throwable

object HttpError:
  type ContentNegotiationFailed = ContentNegotiationFailed.type
  case object ContentNegotiationFailed extends HttpError

  final case class Failure(throwable: Throwable) extends HttpError:
    override def getCause: Throwable = throwable

  type MediaTypeUnsupported = MediaTypeUnsupported.type
  case object MediaTypeUnsupported extends HttpError

  final case class ValidationViolations(violations: Violations) extends HttpError:
    override def getMessage: String = violations.show
