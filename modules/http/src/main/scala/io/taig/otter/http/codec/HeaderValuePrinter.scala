package io.taig.otter.http.codec

import cats.syntax.all.*
import io.taig.otter.codec.Encoder
import io.taig.otter.escape
import io.taig.otter.http.Header

object HeaderValuePrinter extends Encoder[Header.Schema, String]:
  override def encode[A](schema: Header.Schema[A], a: A): String = schema match
    case schema: Header.Schema.Atom[A] => HeaderValueAtomPrinter.encode(schema, a)
    case schema: Header.Schema.Array[A] =>
      HeaderValueArrayEncoder
        .encode(schema, a)
        .map(escape(_, ","))
        .mkString_(",")
    case schema: Header.Schema.Object[A] =>
      HeaderValueObjectEncoder
        .encode(schema, a)
        .map:
          case (key, Some(value)) => s"${escape(key, List(",", "="))}=${escape(value, ",")}"
          case (key, None)        => s"${escape(key, List(",", "="))}"
        .mkString_(",")
