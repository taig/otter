package io.taig.otter.http

import cats.syntax.all.*
import org.http4s.Method as Http4sMethod
import org.http4s.Request as Http4sRequest
import io.taig.otter.http.*
import org.http4s.Uri

object RequestMatcher:
  def apply(reference: Request[?], actual: Http4sRequest[?]): Boolean =
    RequestMatcher(reference.method, actual.method) &&
      RequestMatcher(reference.url, actual.uri)

  def apply(reference: Method, actual: Http4sMethod): Boolean = reference.toString === actual.name

  def apply(reference: Url[?], actual: Uri): Boolean = RequestMatcher(reference.path, actual.path)

  def apply(reference: Path[?], actual: Uri.Path): Boolean =
    RequestMatcher(reference.segments.toList, actual.segments.toList)

  def apply(reference: List[Segment[?]], actual: List[Uri.Path.Segment]): Boolean =
    (reference, actual) match
      case ((Segment.Static(name)) :: reference, y :: actual) if name === y.encoded => RequestMatcher(reference, actual)
      case ((_: Segment.Parameter[?]) :: reference, _ :: actual)                    => RequestMatcher(reference, actual)
      case (Nil, Nil)                                                               => true
      case _                                                                        => false
