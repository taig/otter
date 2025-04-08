package io.taig.otter.http

trait HttpDsl:
  object header extends HttpHeaderDsl

object HttpDsl extends HttpDsl
