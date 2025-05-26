package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.collectFirstWithRemainders

object UrlMatcher:
  def apply(url: Url[?], data: Url.Data): Boolean =
    apply(path = url.path.self, data = data.path).fold(false)(_.isEmpty) &&
      apply(queries = url.queries.self, data = data.queries).isDefined

  def apply(path: Path.Value[?], data: Path.Data): Option[Path.Data] = path match
    case Path.Value.Empty              => data.some
    case Path.Value.Modify(self, _, _) => apply(path = self, data)
    case Path.Value.Root(_) =>
      data.uncons.map((_, tail) => tail)
    case Path.Value.Static(name) =>
      data.uncons.flatMap((head, tail) => Option.when(head === name)(tail))
    case Path.Value.Zip(left, right) => apply(path = left, data).flatMap(apply(path = right, _))

  def apply(queries: Queries.Value[?], data: Queries.Data): Option[Queries.Data] = queries match
    case Queries.Value.Empty              => data.some
    case Queries.Value.Root(query)        => apply(query, data)
    case Queries.Value.Modify(self, _, _) => apply(queries = self, data)
    case Queries.Value.Optional(_)        => data.some
    case Queries.Value.Zip(left, right)   => apply(queries = left, data).flatMap(apply(queries = right, _))

  def apply(query: Query[?], data: Queries.Data): Option[Queries.Data] =
    val name = query.name
    val (remainders, result) = data.collectFirstWithRemainders { case (`name`, _) => () }
    result.as(remainders)
