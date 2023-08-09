//package io.taig.otter.http
//
//import cats.data.{Chain, Validated}
//import cats.syntax.all.*
//import io.taig.otter.schema.Schema.{Collection, Value}
//import io.taig.otter.schema.{+, Encoder, StringEncoder, Violations}
//
//import scala.annotation.tailrec
//
//object HttpEncoder:
//  def response[A](response: Response[A], a: Validated[Violations, A]): Http.Response =
//    a.fold(HttpEncoder.result.encode(response.violations, _), HttpEncoder.results.encode(response.results, _))
//
//  val results: Encoder[Results, Http.Response] = new Encoder:
//    override def encode[A](results: Results[A], a: A): Http.Response = results match
//      case Results.Root(result)          => HttpEncoder.result.encode(result, a)
//      case results: Results.OrElse[?, ?] => encode(results)(a)
//      case Results.Modify(self, _, g)    => encode(self, g(a))
//
//    def encode[A, B](results: Results.OrElse[A, B]): A + B => Http.Response =
//      case Left(a)  => encode(results.left, a)
//      case Right(b) => encode(results.right, b)
//
//  val result: Encoder[Result, Http.Response] = new Encoder:
//    @tailrec
//    override def encode[A](result: Result[A], a: A): Http.Response = result match
//      case result: Result.Root[?, ?] =>
//        val (additionalHeaders, payload) = HttpEncoder.payload.encode(result.body, a._2)
//        Http.Response(
//          result.code,
//          HttpEncoder.headers.encode(result.headers, a._1) ++ additionalHeaders,
//          payload
//        )
//      case Result.Modify(self, _, g) => encode(self, g(a))
//
//  val headers: Encoder[Headers, Http.Headers] = new Encoder:
//    override def encode[A](headers: Headers[A], a: A): Http.Headers = headers match
//      case Headers.Root             => Chain.empty
//      case Headers.One(header)      => HttpEncoder.header.encode(header, a)
//      case Headers.Zip(left, right) => encode(left, a._1) ++ encode(right, a._2)
//
//  val header: Encoder[Header, Http.Headers] = new Encoder:
//    override def encode[A](header: Header[A], a: A): Http.Headers = header.schema.value match
//      case schema: Collection[Value, ?] => StringEncoder.collection.encode(schema, a).orEmpty.tupleLeft(header.name)
//      case schema: Value[?] => Chain.fromOption(StringEncoder.value.encode(schema, a)).tupleLeft(header.name)
//
//  val payload: Encoder[Response.Body, (Http.Headers, Http.Payload)] = new Encoder:
//    override def encode[A](body: Response.Body[A], a: A): (Http.Headers, Http.Payload) = body match
//      case Response.Body.Strict.Bytes                => (Chain.empty, Http.Payload.Strict(a))
//      case Response.Body.Strict.WithHeaders(self)    => encode(self, a._2).leftMap(a._1 ++ _)
//      case Response.Body.Strict.AndThen(self, _, g)  => encode(self, g(a))
//      case Response.Body.Strict.Validate(self, _, g) => encode(self, g(a))
