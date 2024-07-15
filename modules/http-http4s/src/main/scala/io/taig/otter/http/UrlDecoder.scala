package io.taig.otter.http

import org.http4s.Uri as Http4sUri
import io.taig.otter.Decoder
import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.taig.otter.Constraint

object UrlDecoder:
  def apply[A](
      url: Url[A],
      segments: Chain[Http4sUri.Path.Segment],
      parameters: Chain[(String, Option[String])]
  ): Decoder.Result[Option[String], A] = withRemainders(url, segments, parameters)
    .andThen:
      case (Chain.nil, _, a) => a.valid
      case (segments, _, _)  => Violations.rootNec(Violation(Constraint.Type("null"), actual = "/".some)).invalid

  def withRemainders[A](
      url: Url[A],
      segments: Chain[Http4sUri.Path.Segment],
      parameters: Chain[(String, Option[String])]
  ): Decoder.Result[Option[String], (Chain[Http4sUri.Path.Segment], Chain[(String, Option[String])], A)] = url match
    case Url.Combine(left, right) =>
      withRemainders(left, segments, parameters).andThen { case (segments, parameters, a) =>
        withRemainders(right, segments, parameters).map(_.tupleLeft(a))
      }
    case Url.Root(path, queries) =>
      (PathDecoder.withRemainders(path, segments), QueriesDecoder.withRemainders(queries, parameters))
        .mapN { case ((segments, a), (parameters, b)) => (segments, parameters, (a, b)) }
    case Url.Transform(self, f, _) => withRemainders(self, segments, parameters).map(_.map(f))
