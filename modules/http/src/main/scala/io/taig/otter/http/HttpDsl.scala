package io.taig.otter.http

trait HttpDsl extends BodyDsl, MethodDsl, RequestDsl, ResponseDsl, ResultDsl, UrlDsl:
  object code extends CodeDsl

  object header extends HttpHeaderDsl

object HttpDsl extends HttpDsl
