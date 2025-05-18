package io.taig.otter.http.syntax

import io.taig.otter.http.Url

trait UrlSyntax:
  val __ : Url[Unit] = Url.Empty

object UrlSyntax extends UrlSyntax
