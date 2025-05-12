package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Violations

sealed abstract class HttpError extends Throwable

object HttpError:
  type ContentNegotiationFailed = ContentNegotiationFailed.type
  case object ContentNegotiationFailed extends HttpError

  final case class Failure(throwable: Throwable) extends HttpError:
    override def getCause: Throwable = throwable

  type MediaTypeUnsupported = MediaTypeUnsupported.type
  case object MediaTypeUnsupported extends HttpError

  type UrlUnknown = UrlUnknown.type
  case object UrlUnknown extends HttpError

  final case class ValidationViolations(violations: Violations) extends HttpError:
    override def getMessage: String = violations.show
