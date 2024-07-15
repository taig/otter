package io.taig.otter.http

import org.http4s.Uri as Http4sUri
import io.taig.otter.Decoder
import cats.data.Chain

object UrlDecoder:
  def apply[A](
      url: Url[A],
      segments: Chain[Http4sUri.Path.Segment],
      queries: Chain[(String, Option[String])]
  ): Decoder.Result[Option[String], A] = url match
    case Url.Combine(left, right) => ???
    case Url.Root(path, queries) =>
      ???
    case Url.Transform(self, f, _) => UrlDecoder(self, segments, queries).map(f)
