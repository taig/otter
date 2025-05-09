package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.collectFirstWithRemainders
import org.http4s.Query as Http4sQuery
import org.http4s.Uri as Http4sUri

object Http4sUrlMatcher:
  def apply(reference: Url[?], actual: Http4sUri): Boolean =
    apply(reference = reference.path, path = actual.path) &&
      apply(reference = reference.queries, actual = actual.query)

  def apply(reference: Path[?], path: Http4sUri.Path): Boolean =
    apply(reference, segments = path.segments.toList) match
      case Some(Nil)      => true
      case Some(_) | None => false

  def apply(reference: Path[?], segments: List[Http4sUri.Path.Segment]): Option[List[Http4sUri.Path.Segment]] =
    reference match
      case Path.Empty              => segments.some
      case Path.Modify(self, _, _) => apply(reference = self, segments)
      case Path.Root(Segment.Parameter(_, _, _)) =>
        segments match
          case _ :: tail => tail.some
          case _         => none
      case Path.Root(Segment.Static(name, _)) =>
        segments match
          case head :: tail =>
            // TODO should probably be comparing decoded here
            Option.when(head.encoded === name)(tail)
          case _ => none
      case Path.Root(Segment.Modify(self, _, _)) => apply(reference = Path.Root(self), segments)
      case Path.Zip(left, right) =>
        apply(reference = left, segments).flatMap(apply(reference = right, _))

  def apply(reference: Queries[?], actual: Http4sQuery): Boolean =
    apply(reference, values = actual.toList).isDefined

  def apply(reference: Queries[?], values: List[(String, Option[String])]): Option[List[(String, Option[String])]] =
    reference match
      case Queries.Empty              => values.some
      case Queries.Root(query)        => apply(reference = query, values)
      case Queries.Modify(self, _, _) => apply(reference = self, values)
      case Queries.Optional(_)        => values.some
      case Queries.Zip(left, right)   => apply(reference = left, values).flatMap(apply(reference = right, _))

  def apply(reference: Query[?], values: List[(String, Option[String])]): Option[List[(String, Option[String])]] =
    val name = reference.name
    val (remainders, result) = values.collectFirstWithRemainders { case (`name`, _) => () }
    result.as(remainders)
