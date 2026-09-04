package io.taig.otter.http

import cats.Order
import cats.Show

/** A response status code.
  *
  * An `Int` rather than an enumeration, for the reason [[Method]] is a `String`: the registry grows, and a schema that
  * could only name today's codes would describe a smaller protocol than it runs on.
  */
final case class Code(value: Int):
  /** Whether the code says the request succeeded, which is the only classification a schema needs: it is what decides
    * whether a [[Result]] carries what the caller asked for or why it could not have it.
    */
  def isSuccess: Boolean = value >= 200 && value < 300

object Code:
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

  val Ok: Code = Code(200)
  val Created: Code = Code(201)
  val Accepted: Code = Code(202)
  val NoContent: Code = Code(204)
  val MovedPermanently: Code = Code(301)
  val Found: Code = Code(302)
  val NotModified: Code = Code(304)
  val BadRequest: Code = Code(400)
  val Unauthorized: Code = Code(401)
  val Forbidden: Code = Code(403)
  val NotFound: Code = Code(404)
  val MethodNotAllowed: Code = Code(405)
  val NotAcceptable: Code = Code(406)
  val Conflict: Code = Code(409)
  val Gone: Code = Code(410)
  val PayloadTooLarge: Code = Code(413)
  val UnsupportedMediaType: Code = Code(415)
  val UnprocessableEntity: Code = Code(422)
  val TooManyRequests: Code = Code(429)
  val InternalServerError: Code = Code(500)
  val NotImplemented: Code = Code(501)
  val BadGateway: Code = Code(502)
  val ServiceUnavailable: Code = Code(503)
  val GatewayTimeout: Code = Code(504)

  given order: Order[Code] = Order.by(_.value)

  given show: Show[Code] = Show.show(_.value.toString)
