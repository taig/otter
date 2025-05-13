package io.taig.otter.http

import cats.Order

opaque type Code = Int

object Code:
  extension (code: Code) inline def toInt: Int = code
  inline def apply(code: Int): Code = code

  given (using order: Order[Int]): Order[Code] = order

  trait Syntax:
    inline def apply(value: Int): Code = Code(value)
    val ok: Code = 200
    val created: Code = 201
    val accepted: Code = 202
    val noContent: Code = 204
    val partialContent: Code = 206
    val movedPermanently: Code = 301
    val found: Code = 302
    val seeOther: Code = 303
    val notModified: Code = 304
    val temporaryRedirect: Code = 307
    val permanentRedirect: Code = 308
    val badRequest: Code = 400
    val unauthorized: Code = 401
    val forbidden: Code = 403
    val notFound: Code = 404
    val methodNotAllowed: Code = 405
    val notAcceptable: Code = 406
    val conflict: Code = 409
    val gone: Code = 410
    val payloadTooLarge: Code = 413
    val unsupportedMediaTypes: Code = 415
    val unprocessableEntity: Code = 422
    val tooManyRequests: Code = 429
    val internalServerError: Code = 500
    val serviceUnavailable: Code = 503
