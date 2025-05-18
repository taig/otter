package io.taig.otter.http.codec

import io.taig.otter.codec.Encoder
import io.taig.otter.http.Url

object UrlDataEncoder extends Encoder[Url, Url.Data]:
  override def encode[A](url: Url[A], a: A): Url.Data = url match
    case Url.Empty              => Url.Data.Empty
    case Url.Modify(self, _, g) => encode(url = self, g(a))
    case Url.Root(path, queries) =>
      Url.Data(
        path = PathDataEncoder.encode(path, a._1),
        queries = QueriesDataEncoder.encode(queries, a._2)
      )
    case Url.Zip(left, right) => encode(url = left, a._1).combine(encode(url = right, a._2))
