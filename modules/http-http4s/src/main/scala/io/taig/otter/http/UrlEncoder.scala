package io.taig.otter.http

import org.http4s.Uri as Http4sUri

object UrlEncoder:
  def apply[A](url: Url[A], a: A): Http4sUri = ???
