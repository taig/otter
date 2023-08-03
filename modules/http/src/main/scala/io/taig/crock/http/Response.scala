//package io.taig.crock.http
//
//import org.typelevel.ci.CIString
//
//import scala.collection.immutable.VectorMap
//
//final case class Response(code: Code, headers: VectorMap[CIString, String], body: Response.Body):
//  def modifyHeaders(f: VectorMap[CIString, String] => VectorMap[CIString, String]): Response =
//    copy(headers = f(headers))
//  def withHeaders(headers: VectorMap[CIString, String]): Response = modifyHeaders(_ => headers)
//
//object Response:
//  enum Body:
//    case Strict(data: Array[Byte])
//    case Streaming(data: Stream)
//
//    def isEmpty: Boolean = this match
//      case Strict(data)    => data.isEmpty
//      case Streaming(data) => data.isEmpty
//
//  object Body:
//    val Empty: Response.Body = Strict(Array.empty)
//
//    given Encoder[Response.Body] =
//      case _: Strict    => OpenApi.fromString("Strict(...)")
//      case _: Streaming => OpenApi.fromString("Streaming(...)")
//
//  given Encoder[Response] = response =>
//    OpenApi.obj(
//      "code" := response.code,
//      "headers" := OpenApi.fromSeqMap(response.headers.map { case (key, value) => (key.toString, value.asOpenApi) }),
//      "body" := response.body.asOpenApi
//    )
