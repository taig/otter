package io.taig.otter.http

trait HttpDsl extends UrlDsl:
  object header extends HttpHeaderDsl

object HttpDsl extends HttpDsl
