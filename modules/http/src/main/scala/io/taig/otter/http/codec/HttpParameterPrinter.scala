package io.taig.otter.http.codec

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.*
import io.taig.otter.codec.Encoder
import io.taig.otter.http.Parameter
import io.taig.otter.http.Http

final class HttpParameterPrinter(name: String, style: Parameter.Style) extends Encoder[Http.Parameter, String]:
  override def encode[A](schema: Http.Parameter[A], a: A): String = schema match
    case schema: Http.Parameter.Value[A] => HttpParameterValuePrinter.encode(schema, a)
    case schema: Http.Parameter.Array[A] =>
      val values = HttpParameterArrayEncoder.encode(schema, a)

      style match
        case Parameter.Style.Simple => values.map(escape(_, ",")).mkString_(",")
        case Parameter.Style.Label  => values.map(escape(_, ".")).mkString_(".", ".", "")
        case Parameter.Style.Matrix =>
          values.map(value => s"${escape(name, List("=", ";"))}=${escape(value, ";")}").mkString_(";", ";", "")
    case schema: Http.Parameter.Object[A] =>
      val values = HttpParameterObjectEncoder.encode(schema, a)

      style match
        case Parameter.Style.Simple =>
          values
            .map:
              case (key, Some(value)) => s"${escape(key, List("=", ","))}=${escape(value, ",")}"
              case (key, None)        => s"${escape(key, List("=", ","))}"
            .mkString_(",")
        case Parameter.Style.Label =>
          values
            .map:
              case (key, Some(value)) => s"${escape(key, List("=", "."))}=${escape(value, ".")}"
              case (key, None)        => s"${escape(key, List("=", "."))}"
            .mkString_(".", ".", "")
        case Parameter.Style.Matrix =>
          values
            .map:
              case (key, Some(value)) => s"${escape(key, List("=", ";"))}=${escape(value, ";")}"
              case (key, None)        => s"${escape(key, List("=", ";"))}"
            .mkString_(";", ";", "")
