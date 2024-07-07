package io.taig.otter.http

import cats.Id as Identity
import org.http4s.Header as Http4sHeader
import org.http4s.Headers as Http4sHeaders
import io.taig.otter.http.Headers.Writer.Combine
import io.taig.otter.http.Headers.Writer.One
import io.taig.otter.http.Headers.Combine
import io.taig.otter.http.Headers.Empty
import io.taig.otter.http.Headers.One

object HeadersEncoder:
  def apply[A](headers: Headers.Writer[Identity, A], a: A): Http4sHeaders = headers match
    case Headers.Combine(left, right)        => combine(left, right, a)
    case Headers.Empty                       => Http4sHeaders.empty
    case Headers.One(header)                 => one(header, a)
    case Headers.Writer.Combine(left, right) => combine(left, right, a)
    case Headers.Writer.One(header)          => one(header, a)

  def combine[A, B](left: Headers.Writer[Identity, A], right: Headers.Writer[Identity, B], ab: (A, B)): Http4sHeaders =
    HeadersEncoder(left, ab._1) ++ HeadersEncoder(right, ab._2)

  def one[A](header: Header.Writer[Identity, A], a: A): Http4sHeaders =
    HeaderEncoder(header, a).fold(Http4sHeaders.empty)(Http4sHeaders(_))
