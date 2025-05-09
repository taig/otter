package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.collectFirstWithRemainders

object UrlMatcher:
  def apply(url: Url[?], data: Url.Data): Boolean =
    apply(path = url.path, data = data.path).fold(false)(_.isEmpty) &&
      apply(queries = url.queries, data = data.queries).isDefined

  def apply(path: Path[?], data: Path.Data): Option[Path.Data] = path match
    case Path.Empty              => data.some
    case Path.Modify(self, _, _) => apply(path = self, data)
    case Path.Root(Segment.Parameter(_, _, _)) =>
      data.uncons.map((_, tail) => tail)
    case Path.Root(Segment.Static(name, _)) =>
      data.uncons.flatMap((head, tail) => Option.when(head === name)(tail))
    case Path.Root(Segment.Modify(self, _, _)) => apply(path = Path.Root(self), data)
    case Path.Zip(left, right)                 => apply(path = left, data).flatMap(apply(path = right, _))

  def apply(queries: Queries[?], data: Queries.Data): Option[Queries.Data] = queries match
    case Queries.Empty              => data.some
    case Queries.Root(query)        => apply(query, data)
    case Queries.Modify(self, _, _) => apply(queries = self, data)
    case Queries.Optional(_)        => data.some
    case Queries.Zip(left, right)   => apply(queries = left, data).flatMap(apply(queries = right, _))

  def apply(query: Query[?], data: Queries.Data): Option[Queries.Data] =
    val name = query.name
    val (remainders, result) = data.collectFirstWithRemainders { case (`name`, _) => () }
    result.as(remainders)
