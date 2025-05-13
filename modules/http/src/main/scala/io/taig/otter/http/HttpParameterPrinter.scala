package io.taig.otter.http

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.*

final class HttpParameterPrinter(explode: Boolean, style: Header.Style):
  def apply[A](name: String, codec: Http.Parameter[A], a: A): String = codec match
    case codec: Http.Parameter.Array[A]  => apply(name, codec, a)
    case codec: Http.Parameter.Object[A] => apply(name, codec, a)
    case codec: Http.Parameter.Value[A]  => apply(name, codec, a)

  def apply[A](name: String, codec: Http.Parameter.Array[A], a: A): String =
    val values = apply(codec, a)

    (explode, style) match
      case (_, Header.Style.Simple)     => values.map(escape(_, ",")).mkString_(",")
      case (false, Header.Style.Label)  => s".${values.map(escape(_, ",")).mkString_(",")}"
      case (true, Header.Style.Label)   => s".${values.map(escape(_, ".")).mkString_(".")}"
      case (false, Header.Style.Matrix) => s";${escape(name, "=")}=${values.map(escape(_, ",")).mkString_(",")}"
      case (true, Header.Style.Matrix) =>
        values.map(value => s";${escape(name, List(";", "="))}=${escape(value, ";")}").mkString_("")

  def apply[A](codec: Http.Parameter.Array[A], a: A): Chain[String] = codec match
    case Http.Parameter.Array.Collection(self) =>
      Chain.fromSeq(CollectionPrinter(printer = HttpParameterValuePrinter)(codec = self, a))
    case Http.Parameter.Array.Tuple(self) => TuplePrinter(printer = HttpParameterValuePrinter)(codec = self, a)

  def apply[A](name: String, codec: Http.Parameter.Object[A], a: A): String =
    val values = apply(codec, a)

    (explode, style) match
      case (false, Header.Style.Simple) =>
        values.map((name, value) => s"${escape(name, ",")},${escape(value, ",")}").mkString_(",")
      case (true, Header.Style.Simple) =>
        values.map((name, value) => s"${escape(name, List(",", "="))}=${escape(value, ",")}").mkString_(",")
      case (false, Header.Style.Label) =>
        s".${values.map((name, value) => s"${escape(name, ",")},${escape(value, ",")}").mkString_(",")}"
      case (true, Header.Style.Label) =>
        s".${values.map((name, value) => s"${escape(name, List(".", "="))}=${escape(value, ".")}").mkString_(".")}"
      case (false, Header.Style.Matrix) =>
        s";$name=${values.map((name, value) => s"${escape(name, ",")},${escape(value, ",")}").mkString_(",")}"
      case (true, Header.Style.Matrix) =>
        s";${values.map((name, value) => s"${escape(name, "=")}=${escape(value, ";")}").mkString_(";")}"

  def apply[A](codec: Http.Parameter.Object[A], a: A): Chain[(String, String)] = codec match
    case Http.Parameter.Object.Dictionary(self) =>
      Chain.fromSeq(DictionaryPrinter(printer = HttpParameterValuePrinter)(codec = self, a))
    case Http.Parameter.Object.Record(self) =>
      ??? // RecordPrinter(printer = HttpParameterValuePrinter)(codec = self, a)

  def apply[A](name: String, codec: Http.Parameter.Value[A], a: A): String =
    val value = HttpParameterValuePrinter(codec, a)

    style match
      case Header.Style.Simple => value
      case Header.Style.Label  => s".$value"
      case Header.Style.Matrix => s";${escape(name, "=")}=$value"
