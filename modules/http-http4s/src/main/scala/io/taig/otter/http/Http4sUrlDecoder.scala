package io.taig.otter.http

import cats.data.Validated
import io.taig.otter.Violations
import org.http4s.Query as Http4sQuery
import org.http4s.Uri as Http4sUri
import scala.annotation.targetName
import cats.syntax.all.*

object Http4sUrlDecoder:
  def apply[A](
      url: Url[A],
      value: Http4sUri
  ): Validated[Violations, A] =
    apply(url, segments = value.path.segments.toList, values = value.query.toList)
      // TODO fail if not empty
      .andThen((segments, _, a) => Validated.valid(a))

  def apply[A](
      url: Url[A],
      segments: List[Http4sUri.Path.Segment],
      values: List[Http4sQuery.KeyValue]
  ): Validated[Violations, (List[Http4sUri.Path.Segment], List[Http4sQuery.KeyValue], A)] = url match
    case Url.Empty              => (segments, values, ()).valid
    case Url.Modify(self, f, _) => apply(url = self, segments, values).map(_.map(f))
    case Url.Root(path, queries) =>
      Http4sPathDecoder(path, segments).andThen: (segments, a) =>
        Http4sQueriesDecoder(queries, values).map((values, b) => (segments, values, (a, b)))
    case Url.Zip(left, right) =>
      apply(url = left, segments, values).andThen: (segments, values, a) =>
        apply(url = right, segments, values).map: (segments, values, b) =>
          (segments, values, (a, b))
