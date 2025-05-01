package io.taig.otter.http

import org.http4s.Uri as Http4sUri
import org.http4s.Query as Http4sQuery

object Http4sUrlMatcher:
  def apply(reference: Url[?], actual: Http4sUri): Boolean =
    apply(reference = reference.path, actual = actual.path) &&
      apply(reference = reference.queries, actual = actual.query)

  def apply(reference: Path[?], actual: Http4sUri.Path): Boolean = ???

  def apply(reference: Queries[?], actual: Http4sQuery): Boolean = ???
