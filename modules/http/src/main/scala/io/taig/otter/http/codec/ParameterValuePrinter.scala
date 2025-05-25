package io.taig.otter.http.codec

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.*
import io.taig.otter.codec.Encoder
import io.taig.otter.http.Parameter

final class ParameterValuePrinter(name: String, style: Parameter.Style) extends Encoder[Parameter.Value, String]:
  override def encode[A](schema: Parameter.Value[A], a: A): String = schema match
    case schema: Parameter.Value.Atom[A] => ParameterValueAtomPrinter.encode(schema, a)
    case schema: Parameter.Value.Array[A] =>
      val values = ParameterValueArrayEncoder.encode(schema, a)

      style match
        case Parameter.Style.Simple => values.map(escape(_, ",")).mkString_(",")
        case Parameter.Style.Label  => values.map(escape(_, ".")).mkString_(".", ".", "")
        case Parameter.Style.Matrix =>
          values.map(value => s"${escape(name, List("=", ";"))}=${escape(value, ";")}").mkString_(";", ";", "")
    case schema: Parameter.Value.Object[A] =>
      val values = ParameterValueObjectEncoder.encode(schema, a)

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
