package io.taig.otter.http

trait HttpDsl extends AppDsl, BodyDsl, EndpointDsl, RequestDsl, ResponseDsl, ResultsDsl, ResultDsl, UrlDsl:
  object code extends CodeDsl
  object header extends HttpHeaderDsl
  object method extends MethodDsl

object HttpDsl extends HttpDsl
