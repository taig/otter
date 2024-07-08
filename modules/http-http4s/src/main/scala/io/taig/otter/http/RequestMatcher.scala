package io.taig.otter.http

import cats.syntax.all.*
import org.http4s.Header as Http4sHeader
import org.http4s.Headers as Http4sHeaders
import org.http4s.Method as Http4sMethod
import org.http4s.Request as Http4sRequest
import io.taig.otter.http.Plain.*
import org.http4s.Uri

object RequestMatcher:
  def apply(reference: Request.Any, actual: Http4sRequest[?]): Boolean =
    RequestMatcher(reference.method, actual.method)
    ???

  def apply(reference: Method, actual: Http4sMethod): Boolean =
    reference.toString === actual.name

  def apply(url: Url[?], uri: Uri): Boolean = ???
