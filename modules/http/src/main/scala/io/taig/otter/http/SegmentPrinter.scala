package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.*

object SegmentPrinter extends Printer[Segment]:
  override def apply[A](segment: Segment[A], a: A): String = segment match
    case Segment.Static(name) => name
    case Segment.Parameter.Array(name, codec, explode, style) =>
      val values = SegmentCodecArrayPrinter(codec = codec.value, a)

      (style, explode) match
        case (Segment.Style.Label, false)  => "." + values.mkString_(",")
        case (Segment.Style.Label, true)   => "." + values.mkString_(".")
        case (Segment.Style.Matrix, false) => s";$name=" + values.mkString_(",")
        case (Segment.Style.Matrix, true)  => values.map(value => s";$name=$value").mkString_("")
        case (Segment.Style.Simple, _)     => values.mkString_(",")
    case Segment.Parameter.Modify(self, _, g) => apply(segment = self, g(a))
    case Segment.Parameter.Object(name, codec, explode, style) =>
      val values = SegmentCodecObjectPrinter(codec = codec.value, a)

      (style, explode) match
        case (Segment.Style.Label, false)  => "." + values.map((key, value) => s"$key,$value").mkString_(",")
        case (Segment.Style.Label, true)   => "." + values.map((key, value) => s"$key=$value").mkString_(",")
        case (Segment.Style.Matrix, false) => s";$name=" + values.map((key, value) => s"$key=$value").mkString_(",")
        case (Segment.Style.Matrix, true)  => ";" + values.map((key, value) => s"$key=$value").mkString_(",")
        case (Segment.Style.Simple, false) => values.map((key, value) => s"$key,$value").mkString_(",")
        case (Segment.Style.Simple, true)  => values.map((key, value) => s"$key=$value").mkString_(",")
    case Segment.Parameter.Value(name, codec, style) =>
      val value = SegmentCodecPrinter(codec = codec.value, a)

      style match
        case Segment.Style.Label  => s".$value"
        case Segment.Style.Matrix => s";$name=$value"
        case Segment.Style.Simple => value
