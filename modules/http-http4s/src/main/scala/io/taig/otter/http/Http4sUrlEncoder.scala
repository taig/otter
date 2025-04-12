package io.taig.otter.http

import io.taig.otter.Encoder
import org.http4s.Query as Http4sQuery
import org.http4s.Uri as Http4sUri
import org.http4s.syntax.all.*

object Http4sUrlEncoder extends Encoder[Url, Http4sUri]:
  override def apply[A](url: Url[A], a: A): Http4sUri =
    val (path, query) = Raw(url, a)
    Http4sUri(path = path, query = query)

  object Raw extends Encoder[Url, (Http4sUri.Path, Http4sQuery)]:
    def apply[A](url: Url[A], a: A): (Http4sUri.Path, Http4sQuery) = url match
      case Url.Empty              => (Http4sUri.Path.Root, Http4sQuery.Empty)
      case Url.Modify(self, _, g) => apply(url = self, g(a))
      case Url.Root(path, queries) =>
        (Http4sPathEncoder(path, a._1), Http4sQueriesEncoder(queries, a._2))
      case Url.Zip(left, right) =>
        val (leftPath, leftQueries) = apply(url = left, a._1)
        val (rightPath, rightQueries) = apply(url = right, a._2)
        (
          leftPath.concat(rightPath),
          leftQueries ++ rightQueries.toVector
        )
