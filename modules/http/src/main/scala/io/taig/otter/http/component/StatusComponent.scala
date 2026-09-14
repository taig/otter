package io.taig.otter.http.component

import io.taig.otter.http.Status

/** The status codes worth having a name for.
  *
  * Here rather than on [[Status]], for the reason [[MethodComponent]] gives: the type says what a status is, and the
  * dsl says which ones are worth spelling as a word. [[apply]] names the rest, so the list being only the statuses a
  * specification has defined costs nothing.
  */
trait StatusComponent:
  /** A status code by number, which is how a code this list does not name is still answered with. */
  inline def apply(value: Int): Status = Status(value)

  val ok: Status = Status(200)

  val created: Status = Status(201)

  val accepted: Status = Status(202)

  val noContent: Status = Status(204)

  val movedPermanently: Status = Status(301)

  val found: Status = Status(302)

  val notModified: Status = Status(304)

  val badRequest: Status = Status(400)

  val unauthorized: Status = Status(401)

  val forbidden: Status = Status(403)

  val notFound: Status = Status(404)

  val methodNotAllowed: Status = Status(405)

  val notAcceptable: Status = Status(406)

  val conflict: Status = Status(409)

  val gone: Status = Status(410)

  val payloadTooLarge: Status = Status(413)

  val unsupportedMediaType: Status = Status(415)

  val unprocessableEntity: Status = Status(422)

  val tooManyRequests: Status = Status(429)

  val internalServerError: Status = Status(500)

  val notImplemented: Status = Status(501)

  val badGateway: Status = Status(502)

  val serviceUnavailable: Status = Status(503)

  val gatewayTimeout: Status = Status(504)
