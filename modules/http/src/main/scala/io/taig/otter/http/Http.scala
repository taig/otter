package io.taig.otter.http

import cats.syntax.all.*
import org.typelevel.ci.CIString
import cats.Show
import fs2.Stream

object Http:
  type Path = Vector[String]

  object Path:
    val Empty: Http.Path = Vector.empty

  type Queries = Vector[(String, Option[String])]

  object Queries:
    val Empty: Http.Queries = Vector.empty

  final case class Url(path: Http.Path, queries: Http.Queries):
    def ++(url: Http.Url): Http.Url = Url(path ++ url.path, queries ++ url.queries)

    override def toString: String =
      path.mkString_("/", "/", "") +
        (if queries.isEmpty then "" else queries.map { case (key, value) => s"$key=$value" }.mkString_("?", "&", ""))

  object Url:
    val Empty: Http.Url = Url(Vector.empty, Vector.empty)

    given Show[Http.Url] = Show.fromToString

  type Headers = Vector[(CIString, String)]

  object Headers:
    val Empty: Http.Headers = Vector.empty

  final case class Request[F[_]](method: Method, url: Http.Url, headers: Http.Headers, body: Stream[F, Byte]):
    def modifyMethod(f: Method => Method): Http.Request[F] = copy(method = f(method))
    def withMethod(method: Method): Http.Request[F] = modifyMethod(_ => method)

    def modifyUrl(f: Http.Url => Http.Url): Http.Request[F] = copy(url = f(url))
    def withUrl(url: Http.Url): Http.Request[F] = modifyUrl(_ => url)

    def modifyHeaders(f: Http.Headers => Http.Headers): Http.Request[F] = copy(headers = f(headers))
    def withHeaders(headers: Http.Headers): Http.Request[F] = modifyHeaders(_ => headers)

  final case class Response[F[_]](code: Code, headers: Http.Headers, body: Stream[F, Byte]):
    def modifyCode(f: Code => Code): Http.Response[F] = copy(code = f(code))
    def withCode(code: Code): Http.Response[F] = modifyCode(_ => code)

    def modifyHeaders(f: Http.Headers => Http.Headers): Http.Response[F] = copy(headers = f(headers))
    def withHeaders(headers: Http.Headers): Http.Response[F] = modifyHeaders(_ => headers)
