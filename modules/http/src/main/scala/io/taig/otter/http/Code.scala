package io.taig.otter.http

import cats.Order
import cats.Show

/** A response status code. */
opaque type Code = Int

object Code:
  extension (self: Code)
    inline def value: Int = self

    /** Whether the code says the request succeeded, which is the only classification a schema needs: it is what decides
      * whether a [[Result]] carries what the caller asked for or why it could not have it.
      */
    def isSuccess: Boolean = self >= 200 && self < 300

  inline def apply(value: Int): Code = value

  /** The phrase the specification gives this code, where it gives one.
    *
    * Here rather than in a renderer because it is a fact about the code and not about any document: an OpenAPI response
    * needs a description, a log line wants the same words, and neither should have its own table.
    */
  def reason(code: Code): Option[String] = Code.reasons.get(code.value)

  private val reasons: Map[Int, String] = Map(
    200 -> "OK",
    201 -> "Created",
    202 -> "Accepted",
    204 -> "No Content",
    301 -> "Moved Permanently",
    302 -> "Found",
    304 -> "Not Modified",
    400 -> "Bad Request",
    401 -> "Unauthorized",
    403 -> "Forbidden",
    404 -> "Not Found",
    405 -> "Method Not Allowed",
    406 -> "Not Acceptable",
    409 -> "Conflict",
    410 -> "Gone",
    413 -> "Content Too Large",
    415 -> "Unsupported Media Type",
    422 -> "Unprocessable Content",
    429 -> "Too Many Requests",
    500 -> "Internal Server Error",
    501 -> "Not Implemented",
    502 -> "Bad Gateway",
    503 -> "Service Unavailable",
    504 -> "Gateway Timeout"
  )

  given order: Order[Code] = Order.by(_.value)

  given show: Show[Code] = Show.show(_.value.toString)
