package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.*
import io.taig.otter.Discriminator.Explicit
import io.taig.otter.Discriminator.Merged

object Http
//   type Path = Vector[String]

//   object Path:
//     val Empty: Http.Path = Vector.empty

//   type Queries = Vector[(String, Option[String])]

//   object Queries:
//     val Empty: Http.Queries = Vector.empty

//   final case class Url(path: Http.Path, queries: Http.Queries):
//     def ++(url: Http.Url): Http.Url = Url(path ++ url.path, queries ++ url.queries)

//     override def toString: String = path.mkString_("/", "/", "") +
//       (if queries.isEmpty then ""
//        else
//          queries
//            .map {
//              case (key, Some(value)) => s"$key=$value"
//              case (key, None)        => key
//            }
//            .mkString_("?", "&", ""))

//   object Url:
//     val Empty: Http.Url = Url(Vector.empty, Vector.empty)

//     given Show[Http.Url] = Show.fromToString

//   type Headers = Vector[(CIString, String)]

//   object Headers:
//     val Empty: Http.Headers = Vector.empty

//   final case class Request(method: Method, url: Http.Url, headers: Http.Headers, body: Array[Byte]):
//     def modifyMethod(f: Method => Method): Http.Request = copy(method = f(method))
//     def withMethod(method: Method): Http.Request = modifyMethod(_ => method)

//     def modifyUrl(f: Http.Url => Http.Url): Http.Request = copy(url = f(url))
//     def withUrl(url: Http.Url): Http.Request = modifyUrl(_ => url)

//     def modifyHeaders(f: Http.Headers => Http.Headers): Http.Request = copy(headers = f(headers))
//     def withHeaders(headers: Http.Headers): Http.Request = modifyHeaders(_ => headers)

//   final case class Response(code: Code, headers: Http.Headers, body: Array[Byte]):
//     def modifyCode(f: Code => Code): Http.Response = copy(code = f(code))
//     def withCode(code: Code): Http.Response = modifyCode(_ => code)

//     def modifyHeaders(f: Http.Headers => Http.Headers): Http.Response = copy(headers = f(headers))
//     def withHeaders(headers: Http.Headers): Http.Response = modifyHeaders(_ => headers)
