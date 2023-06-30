package io.taig.openapi.http

import cats.data.{Chain, NonEmptyChain}
import cats.syntax.all.*
import org.typelevel.ci.CIString

object Http:
  opaque type Path = Chain[String]
  object Path:
    extension (self: Http.Path) def toChain: Chain[String] = self
    val Root: Http.Path = Chain.empty

  opaque type Headers = Chain[(CIString, String)]
  object Headers:
    extension (self: Http.Headers)
      def toChain: Chain[(CIString, String)] = self
      def contains(name: CIString): Boolean = toChain.exists { case (current, _) => name === current }
      def get(name: CIString): Option[NonEmptyChain[String]] =
        NonEmptyChain.fromChain(toChain.collect { case (`name`, value) => value })
      def getFirst(name: CIString): Option[String] = toChain.collectFirst { case (`name`, value) => value }
      def remove(name: CIString): Http.Headers = toChain.filter {
        case (`name`, _) => false
        case _           => true
      }
      def removeFirst(name: CIString): Http.Headers =
        var removed = false
        val result = List.newBuilder[(CIString, String)]
        toChain.iterator.foreach {
          case (`name`, _) if !removed => removed = true; ()
          case entry                   => result += entry
        }
        Chain.fromSeq(result.result())
      def getWithRemainders(name: CIString): Option[(NonEmptyChain[String], Http.Headers)] =
        get(name).tupleRight(remove(name))
      def getFirstWithRemainders(name: CIString): Option[(String, Http.Headers)] =
        getFirst(name).tupleRight(removeFirst(name))
      def ++(headers: Http.Headers): Http.Headers = self.toChain ++ headers.toChain

    val Empty: Http.Headers = Chain.empty
    def apply(headers: Chain[(CIString, String)]): Http.Headers = headers
    def one(name: CIString, value: String): Http.Headers = Chain.one((name, value))

  opaque type Queries = Chain[(String, String)]
  object Queries:
    extension (self: Http.Queries)
      def toChain: Chain[(String, String)] = self
      def contains(name: String): Boolean = toChain.exists { case (current, _) => name === current }
      def get(name: String): Option[NonEmptyChain[String]] =
        NonEmptyChain.fromChain(toChain.collect { case (`name`, value) => value })
      def getFirst(name: String): Option[String] = toChain.collectFirst { case (`name`, value) => value }
      def remove(name: String): Http.Queries = toChain.filter {
        case (`name`, _) => false
        case _           => true
      }
      def removeFirst(name: String): Http.Queries =
        var removed = false
        val result = List.newBuilder[(String, String)]
        toChain.iterator.foreach {
          case (`name`, _) if !removed => removed = true; ()
          case entry                   => result += entry
        }
        Chain.fromSeq(result.result())
      def getWithRemainders(name: String): Option[(NonEmptyChain[String], Http.Queries)] =
        get(name).tupleRight(remove(name))
      def getFirstWithRemainders(name: String): Option[(String, Http.Queries)] =
        getFirst(name).tupleRight(removeFirst(name))
      infix def merge(queries: Http.Queries): Http.Queries = ???

    val Empty: Http.Queries = Chain.empty
    def apply(queries: Chain[(String, String)]): Http.Queries = queries
    def one(name: String, value: String): Http.Queries = Chain.one((name, value))

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
      case Singlepart(entity: Entity[Byte])
      case Multipart

  final case class Response(code: Int, headers: Http.Headers, body: Http.Response.Body):
    def modifyCode(f: Int => Int): Http.Response = copy(code = f(code))
    def withCode(code: Int): Http.Response = modifyCode(_ => code)

    def modifyHeaders(f: Http.Headers => Http.Headers): Http.Response = copy(headers = f(headers))
    def withHeaders(headers: Http.Headers): Http.Response = modifyHeaders(_ => headers)

    def modifyBody(f: Http.Response.Body => Http.Response.Body): Http.Response = copy(body = f(body))
    def withBody(body: Http.Response.Body): Http.Response = modifyBody(_ => body)

  object Response:
    final case class Body(entity: Entity[Byte])
