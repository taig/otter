package io.taig.otter.http

trait HttpDsl extends BodyDsl, EndpointDsl, RequestDsl, ResponseDsl, ResultDsl, UrlDsl:
  object code extends CodeDsl
  object header extends HttpHeaderDsl
  object method extends MethodDsl

object HttpDsl extends HttpDsl
