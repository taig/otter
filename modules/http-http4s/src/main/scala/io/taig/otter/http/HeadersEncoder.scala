package io.taig.otter.http

import org.http4s.Header as Http4sHeader
import org.http4s.Headers as Http4sHeaders
import io.taig.otter.http.*

object HeadersEncoder:
  def apply[A](headers: Headers[A], a: A): Http4sHeaders = headers match
    case Headers.Combine(left, right) => HeadersEncoder(left, a._1) ++ HeadersEncoder(right, a._2)
    case Headers.Empty                => Http4sHeaders.empty
    case Headers.One(header)          => HeaderEncoder(header, a).fold(Http4sHeaders.empty)(Http4sHeaders(_))
