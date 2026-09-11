package io.taig.otter.http.component

import io.taig.otter.http.Code

/** The status codes worth having a name for.
  *
  * Here rather than on [[Code]], for the reason [[MethodComponent]] gives: the type says what a status code is, and the
  * dsl says which ones are worth spelling as a word. [[apply]] names the rest, so the list being only the codes a
  * specification has defined costs nothing.
  */
trait CodeComponent:
  /** A status code by number, which is how a code this list does not name is still answered with. */
  inline def apply(value: Int): Code = Code(value)

  val ok: Code = Code(200)

  val created: Code = Code(201)

  val accepted: Code = Code(202)

  val noContent: Code = Code(204)

  val movedPermanently: Code = Code(301)

  val found: Code = Code(302)

  val notModified: Code = Code(304)

  val badRequest: Code = Code(400)

  val unauthorized: Code = Code(401)

  val forbidden: Code = Code(403)

  val notFound: Code = Code(404)

  val methodNotAllowed: Code = Code(405)

  val notAcceptable: Code = Code(406)

  val conflict: Code = Code(409)

  val gone: Code = Code(410)

  val payloadTooLarge: Code = Code(413)

  val unsupportedMediaType: Code = Code(415)

  val unprocessableEntity: Code = Code(422)

  val tooManyRequests: Code = Code(429)

  val internalServerError: Code = Code(500)

  val notImplemented: Code = Code(501)

  val badGateway: Code = Code(502)

  val serviceUnavailable: Code = Code(503)

  val gatewayTimeout: Code = Code(504)
