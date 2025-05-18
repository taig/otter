package io.taig.otter.http.syntax

import io.taig.otter.http.Code

trait CodeSyntax:
  def apply(value: Int): Code = Code(value)

  val ok: Code = apply(200)
  val created: Code = apply(201)
  val accepted: Code = apply(202)
  val noContent: Code = apply(204)
  val partialContent: Code = apply(206)
  val movedPermanently: Code = apply(301)
  val found: Code = apply(302)
  val seeOther: Code = apply(303)
  val notModified: Code = apply(304)
  val temporaryRedirect: Code = apply(307)
  val permanentRedirect: Code = apply(308)
  val badRequest: Code = apply(400)
  val unauthorized: Code = apply(401)
  val forbidden: Code = apply(403)
  val notFound: Code = apply(404)
  val methodNotAllowed: Code = apply(405)
  val notAcceptable: Code = apply(406)
  val conflict: Code = apply(409)
  val gone: Code = apply(410)
  val payloadTooLarge: Code = apply(413)
  val unsupportedMediaTypes: Code = apply(415)
  val unprocessableEntity: Code = apply(422)
  val tooManyRequests: Code = apply(429)
  val internalServerError: Code = apply(500)
  val serviceUnavailable: Code = apply(503)

object CodeSyntax extends CodeSyntax
