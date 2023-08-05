package io.taig.otter.http

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.schema.{Encoder, StringEncoder}
import io.taig.otter.schema.Schema.Collection
import io.taig.otter.schema.Schema.Value

object HttpEncoder:
  val response: Encoder[Response, Http.Response] = new Encoder:
    override def encode[A](response: Response[A], a: A): Http.Response = Http.Response(
      code = ???,
      headers = ???,
      body = ???
    )

  val headers: Encoder[Headers, Http.Headers] = new Encoder:
    override def encode[A](headers: Headers[A], a: A): Http.Headers = headers match
      case Headers.Root             => Chain.empty
      case Headers.One(header)      => HttpEncoder.header.encode(header, a)
      case Headers.Zip(left, right) => encode(left, a._1) ++ encode(right, a._2)

  val header: Encoder[Header, Http.Headers] = new Encoder:
    override def encode[A](header: Header[A], a: A): Http.Headers = header.schema.value match
      case schema: Collection[Value, ?] => StringEncoder.collection.encode(schema, a).orEmpty.tupleLeft(header.name)
      case schema: Value[?] => Chain.fromOption(StringEncoder.value.encode(schema, a)).tupleLeft(header.name)
