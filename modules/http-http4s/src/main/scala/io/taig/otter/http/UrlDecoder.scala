package io.taig.otter.http

import org.http4s.Uri as Http4sUri
import io.taig.otter.Decoder

object UrlDecoder:
  def apply[A](url: Url[A], value: Http4sUri): Decoder.Result[Option[String], A] = ???
