package io.taig.otter.http

import cats.data.Chain
import cats.syntax.all.*
import org.typelevel.ci.CIString

import scala.collection.immutable.VectorMap

object Http:
  type Path = Chain[String]

  type Queries = Chain[(String, String)]

  final case class Url(path: Http.Path, queries: Http.Queries)

  opaque type Headers = VectorMap[CIString, Http.Headers.Value]

  object Headers:
    enum Value:
      case Single(value: String)
      case Multiple(values: Chain[String])

  final case class Request(
      method: Method,
      url: Http.Url,
      headers: Http.Headers,
      body: Http.Request.Body
  ):
    def modifyMethod(f: Method => Method): Http.Request = copy(method = f(method))
    def withMethod(method: Method): Http.Request = modifyMethod(_ => method)

    def modifyUrl(f: Http.Url => Http.Url): Http.Request = copy(url = f(url))
    def withUrl(url: Http.Url): Http.Request = modifyUrl(_ => url)

    def modifyHeaders(f: Http.Headers => Http.Headers): Http.Request = copy(headers = f(headers))
    def withHeaders(headers: Http.Headers): Http.Request = modifyHeaders(_ => headers)

    def modifyBody(f: Http.Request.Body => Http.Request.Body): Http.Request = copy(body = f(body))
    def withBody(body: Http.Request.Body): Http.Request = modifyBody(_ => body)

  object Request:
    sealed abstract class Body extends Product with Serializable

    object Body:
      final case class Singlepart(payload: Http.Payload) extends Request.Body

  final case class Response(code: Code, headers: Http.Headers, body: Http.Payload):
    def modifyCode(f: Code => Code): Http.Response = copy(code = f(code))
    def withCode(code: Code): Http.Response = modifyCode(_ => code)

    def modifyHeaders(f: Http.Headers => Http.Headers): Http.Response = copy(headers = f(headers))
    def withHeaders(headers: Http.Headers): Http.Response = modifyHeaders(_ => headers)

    def modifyBody(f: Http.Payload => Http.Payload): Http.Response = copy(body = f(body))
    def withBody(body: Http.Payload): Http.Response = modifyBody(_ => body)

  enum Payload:
    case Strict(data: Array[Byte])
    case Streaming(stream: Stream[Byte])
