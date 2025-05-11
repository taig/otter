package io.taig.otter.http

import io.taig.otter.Violations

sealed abstract class HttpError extends Throwable

object HttpError:
  type ContentNegotiationFailed = ContentNegotiationFailed.type
  case object ContentNegotiationFailed extends HttpError

  final case class Failure(throwable: Throwable) extends HttpError

  type MediaTypeUnsupported = MediaTypeUnsupported.type
  case object MediaTypeUnsupported extends HttpError

  final case class ValidationViolations(violations: Violations) extends HttpError
