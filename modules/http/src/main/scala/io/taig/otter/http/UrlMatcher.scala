package io.taig.otter.http

import cats.syntax.all.*

object UrlMatcher:
  def apply(reference: Url[?], actual: Url[?]): Boolean =
    apply(reference = reference.path, actual = actual.path) &&
      apply(reference = reference.queries, actual = actual.queries)

  def apply(reference: Path[?], actual: Path[?]): Boolean = (reference, actual) match
    case (Path.Empty, Path.Empty)                  => true
    case (Path.Modify(self, _, _), path)           => apply(reference = self, actual = path)
    case (path, Path.Modify(self, _, _))           => apply(reference = path, actual = self)
    case (Path.Root(reference), Path.Root(actual)) => apply(reference, actual)
    case (Path.Zip(x1, x2), Path.Zip(y1, y2)) =>
      apply(reference = x1, actual = y1) && apply(reference = x2, actual = y2)
    case _ => false

  def apply(reference: Segment[?], actual: Segment[?]): Boolean = (reference, actual) match
    case (Segment.Modify(self, _, _), segment)                                 => apply(reference = self, segment)
    case (segment, Segment.Modify(self, _, _))                                 => apply(reference = segment, self)
    case (Segment.Static(reference, _), Segment.Static(actual, _))             => reference === actual
    case (Segment.Parameter(reference, _, _), Segment.Parameter(actual, _, _)) => reference === actual
    case _                                                                     => false

  def apply(reference: Queries[?], actual: Queries[?]): Boolean = (reference, actual) match
    case (Queries.Empty, Queries.Empty)                  => true
    case (Queries.Modify(self, _, _), queries)           => apply(reference = self, actual = queries)
    case (queries, Queries.Modify(self, _, _))           => apply(reference = queries, actual = self)
    case (Queries.Root(reference), Queries.Root(actual)) => apply(reference, actual)
    case (Queries.Zip(x1, x2), Queries.Zip(y1, y2)) =>
      apply(reference = x1, actual = y1) && apply(reference = x2, actual = y2)
    case _ => false

  def apply(reference: Query[?], actual: Query[?]): Boolean = (reference, actual) match
    case (Query.Modify(self, _, _), query)                       => apply(reference = self, actual = query)
    case (query, Query.Modify(self, _, _))                       => apply(reference = query, actual = self)
    case (Query.Root(reference, _, _), Query.Root(actual, _, _)) => reference === actual
    case (Query.Optional(_), Query.Optional(_))                  => true
    case _                                                       => false
