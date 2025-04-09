package io.taig.otter.http

import cats.syntax.all.*

object HttpSegmentPrinter:
  def apply[A](name: String, codec: Http.Segment[A], a: A, explode: Boolean, style: Header.Style): String =
    codec match
      case codec: Http.Segment.Value[A]  => apply(name, codec, a, style)
      case codec: Http.Segment.Array[A]  => apply(name, codec, a, explode, style)
      case codec: Http.Segment.Object[A] => apply(name, codec, a, explode, style)

  def apply[A](
      name: String,
      codec: Http.Segment.Array[A],
      a: A,
      explode: Boolean,
      style: Header.Style
  ): String =
    val values = HttpSegmentArrayPrinter(codec, a)

    (explode, style) match
      case (_, Header.Style.Simple)     => values.mkString_(",")
      case (false, Header.Style.Label)  => s".${values.mkString_(",")}"
      case (true, Header.Style.Label)   => s".${values.mkString_(".")}"
      case (false, Header.Style.Matrix) => s";$name=${values.mkString_(",")}"
      case (true, Header.Style.Matrix)  => values.map(value => s";$name=$value").mkString_("")

  def apply[A](
      name: String,
      codec: Http.Segment.Object[A],
      a: A,
      explode: Boolean,
      style: Header.Style
  ): String =
    val values = HttpSegmentObjectPrinter(codec, a)

    (explode, style) match
      case (false, Header.Style.Simple) => values.map((name, value) => s"$name,$value").mkString_(",")
      case (true, Header.Style.Simple)  => values.map((name, value) => s"$name=$value").mkString_(",")
      case (false, Header.Style.Label)  => s".${values.map((name, value) => s"$name,$value").mkString_(",")}"
      case (true, Header.Style.Label)   => s".${values.map((name, value) => s"$name=$value").mkString_(".")}"
      case (false, Header.Style.Matrix) => s";$name=${values.map((name, value) => s"$name,$value").mkString_(",")}"
      case (true, Header.Style.Matrix)  => s";${values.map((name, value) => s"$name=$value").mkString_(";")}"

  def apply[A](name: String, codec: Http.Segment.Value[A], a: A, style: Header.Style): String =
    val value = HttpSegmentValuePrinter(codec, a)

    style match
      case Header.Style.Simple => value
      case Header.Style.Label  => s".$value"
      case Header.Style.Matrix => s";$name=$value"
