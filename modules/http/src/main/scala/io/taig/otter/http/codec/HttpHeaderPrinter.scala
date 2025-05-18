package io.taig.otter.http.codec

import cats.syntax.all.*
import io.taig.otter.codec.Encoder
import io.taig.otter.escape
import io.taig.otter.http.Http

object HttpHeaderPrinter extends Encoder[Http.Header, String]:
  override def encode[A](schema: Http.Header[A], a: A): String = schema match
    case schema: Http.Header.Value[A] => HttpHeaderValuePrinter.encode(schema, a)
    case schema: Http.Header.Array[A] =>
      HttpHeaderArrayEncoder
        .encode(schema, a)
        .map(escape(_, ","))
        .mkString_(",")
    case schema: Http.Header.Object[A] =>
      HttpHeaderObjectEncoder
        .encode(schema, a)
        .map:
          case (key, Some(value)) => s"${escape(key, List(",", "="))}=${escape(value, ",")}"
          case (key, None)        => s"${escape(key, List(",", "="))}"
        .mkString_(",")
