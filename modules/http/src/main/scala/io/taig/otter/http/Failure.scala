package io.taig.otter.http

import io.taig.otter.Violations

/** A recoverable failure while interpreting an HTTP endpoint. Causes are diagnostic, never a response body. */
final case class Failure(
    category: Failure.Category,
    violations: Option[Violations] = None,
    cause: Option[Throwable] = None
)

object Failure:
  /** What went wrong, which is what an [[ErrorPolicy]] chooses its answer by.
    *
    * `ContentTooLarge` is a body larger than a backend will read whole. It is apart from `EntityRead` because the
    * request caused it, not the transport, and so it is answered with a client error where a failed read is a server
    * one.
    *
    * `LengthRequired` is a body that has to say how long it is before it is read, as a sized raw byte stream does, and
    * did not: no `Content-Length`, or one a chunked request carries and RFC 9112 says to ignore.
    */
  enum Category:
    case Envelope, Syntax, ContentType, Validation, LengthRequired, ContentTooLarge, EntityRead, Encoding, Status,
      Unexpected
