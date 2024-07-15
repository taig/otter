package io.taig.otter.http

import org.http4s.Uri as Http4sUri
import cats.data.Chain

object UrlEncoder:
  def apply[A](url: Url[A], a: A): (Chain[Http4sUri.Path.Segment], Chain[(String, Option[String])]) = url match
    case Url.Combine(left, right) =>
      val (path1, queries1) = UrlEncoder(left, a._1)
      val (path2, queries2) = UrlEncoder(right, a._2)
      (path1 ++ path2, queries1 ++ queries2)
    case Url.Root(path, queries)   => (PathEncoder(path, a._1), QueriesEncoder(queries, a._2))
    case Url.Transform(self, _, f) => UrlEncoder(self, f(a))
