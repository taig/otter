package io.taig.otter.http

import cats.syntax.all.*
import org.http4s.Method as Http4sMethod
import org.http4s.Request as Http4sRequest
import io.taig.otter.http.Plain.*
import org.http4s.Uri

object RequestMatcher:
  def apply(reference: Request.Any, actual: Http4sRequest[?]): Boolean =
    RequestMatcher(reference.method, actual.method) &&
      RequestMatcher(reference.url, actual.uri)

  def apply(reference: Method, actual: Http4sMethod): Boolean = reference.toString === actual.name

  def apply(reference: Url[?], actual: Uri): Boolean = RequestMatcher(reference.path, actual.path)

  def apply(reference: Path[?], actual: Uri.Path): Boolean =
    RequestMatcher(reference.toSegments.toList, actual.segments.toList)

  def apply(reference: List[String | Path.Parameter[?]], actual: List[Uri.Path.Segment]): Boolean =
    (reference, actual) match
      case ((x: String) :: reference, y :: actual) if x === y.encoded => RequestMatcher(reference, actual)
      case ((_: Path.Parameter[?]) :: reference, _ :: actual)         => RequestMatcher(reference, actual)
      case (Nil, Nil)                                                 => true
      case _                                                          => false
