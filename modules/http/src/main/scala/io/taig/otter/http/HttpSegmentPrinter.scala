package io.taig.otter.http

import cats.syntax.all.*

object HttpSegmentPrinter:
  def apply[A](name: String, codec: Http.Segment[A], a: A, explode: Boolean, serialization: Serialization): String =
    codec match
      case codec: Http.Segment.Value[A]  => apply(name, codec, a, serialization)
      case codec: Http.Segment.Array[A]  => apply(name, codec, a, explode, serialization)
      case codec: Http.Segment.Object[A] => apply(name, codec, a, explode, serialization)

  def apply[A](
      name: String,
      codec: Http.Segment.Array[A],
      a: A,
      explode: Boolean,
      serialization: Serialization
  ): String =
    val values = HttpSegmentArrayPrinter(codec, a)

    (explode, serialization) match
      case (_, Serialization.Simple)     => values.mkString_(",")
      case (false, Serialization.Label)  => s".${values.mkString_(",")}"
      case (true, Serialization.Label)   => s".${values.mkString_(".")}"
      case (false, Serialization.Matrix) => s";$name=${values.mkString_(",")}"
      case (true, Serialization.Matrix)  => values.map(value => s";$name=$value").mkString_("")

  def apply[A](
      name: String,
      codec: Http.Segment.Object[A],
      a: A,
      explode: Boolean,
      serialization: Serialization
  ): String =
    val values = HttpSegmentObjectPrinter(codec, a)

    (explode, serialization) match
      case (false, Serialization.Simple) => values.map((name, value) => s"$name,$value").mkString_(",")
      case (true, Serialization.Simple)  => values.map((name, value) => s"$name=$value").mkString_(",")
      case (false, Serialization.Label)  => s".${values.map((name, value) => s"$name,$value").mkString_(",")}"
      case (true, Serialization.Label)   => s".${values.map((name, value) => s"$name=$value").mkString_(".")}"
      case (false, Serialization.Matrix) => s";$name=${values.map((name, value) => s"$name,$value").mkString_(",")}"
      case (true, Serialization.Matrix)  => s";${values.map((name, value) => s"$name=$value").mkString_(";")}"

  def apply[A](name: String, codec: Http.Segment.Value[A], a: A, serialization: Serialization): String =
    val value = HttpSegmentValuePrinter(codec, a)

    serialization match
      case Serialization.Simple => value
      case Serialization.Label  => s".$value"
      case Serialization.Matrix => s";$name=$value"
