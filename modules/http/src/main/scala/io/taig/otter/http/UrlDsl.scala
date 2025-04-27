package io.taig.otter.http

trait UrlDsl:
  val __ : Url[Unit] = Url.Empty

object UrlDsl extends UrlDsl
