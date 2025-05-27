package io.taig.otter.http.codec

import cats.syntax.all.*
import io.taig.otter.codec.Encoder
import io.taig.otter.escape
import io.taig.otter.http.Header

object HeaderSchemaPrinter extends Encoder[Header.Schema, String]:
  override def encode[A](schema: Header.Schema[A], a: A): String = schema match
    case schema: Header.Schema.Value[A] => HeaderSchemaValuePrinter.encode(schema, a)
    case schema: Header.Schema.Array[A] =>
      HeaderSchemaArrayEncoder
        .encode(schema, a)
        .map(escape(_, ","))
        .mkString_(",")
    case schema: Header.Schema.Object[A] =>
      HeaderSchemaObjectEncoder
        .encode(schema, a)
        .map:
          case (key, Some(value)) => s"${escape(key, List(",", "="))}=${escape(value, ",")}"
          case (key, None)        => s"${escape(key, List(",", "="))}"
        .mkString_(",")
