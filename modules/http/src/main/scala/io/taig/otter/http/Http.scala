package io.taig.otter.http

import cats.data.{Chain, NonEmptyChain}
import cats.syntax.all.*
import org.typelevel.ci.CIString

import scala.collection.immutable.SortedMap

object Http:
  type Path = Chain[String]
  type Headers = Chain[(CIString, String)]
  type Queries = Chain[(String, String)]

  final case class Request(
      method: String,
      path: Http.Path,
      queries: Http.Queries,
      headers: Http.Headers,
      body: Http.Request.Body
  ):
    def modifyMethod(f: String => String): Http.Request = copy(method = f(method))
    def withMethod(method: String): Http.Request = modifyMethod(_ => method)

    def modifyPath(f: Http.Path => Http.Path): Http.Request = copy(path = f(path))
    def withPath(path: Http.Path): Http.Request = modifyPath(_ => path)

    def modifyQueries(f: Http.Queries => Http.Queries): Http.Request = copy(queries = f(queries))
    def withQueries(queries: Http.Queries): Http.Request = modifyQueries(_ => queries)

    def modifyHeaders(f: Http.Headers => Http.Headers): Http.Request = copy(headers = f(headers))
    def withHeaders(headers: Http.Headers): Http.Request = modifyHeaders(_ => headers)

    def modifyBody(f: Http.Request.Body => Http.Request.Body): Http.Request = copy(body = f(body))
    def withBody(body: Http.Request.Body): Http.Request = modifyBody(_ => body)

  object Request:
    enum Body:
      case Singlepart(entity: Stream)
      case Multipart

//  final case class Response(code: Int, headers: Http.Headers, body: Http.Response.Body):
//    def modifyCode(f: Int => Int): Http.Response = copy(code = f(code))
//    def withCode(code: Int): Http.Response = modifyCode(_ => code)
//
//    def modifyHeaders(f: Http.Headers => Http.Headers): Http.Response = copy(headers = f(headers))
//    def withHeaders(headers: Http.Headers): Http.Response = modifyHeaders(_ => headers)
//
//    def modifyBody(f: Http.Response.Body => Http.Response.Body): Http.Response = copy(body = f(body))
//    def withBody(body: Http.Response.Body): Http.Response = modifyBody(_ => body)
//
//  object Response:
//    final case class Body(entity: Stream)
