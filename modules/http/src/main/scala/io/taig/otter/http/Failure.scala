package io.taig.otter.http

import io.taig.otter.Violations

/** A recoverable failure while interpreting an HTTP endpoint. Causes are diagnostic, never a response body. */
final case class Failure(
    category: Failure.Category,
    violations: Option[Violations] = None,
    cause: Option[Throwable] = None
)

object Failure:
  enum Category:
    case Envelope, Syntax, ContentType, Validation, EntityRead, Encoding, Status, Unexpected
