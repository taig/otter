package io.taig.otter.http
import io.taig.otter.*
import io.taig.otter.http.HttpKeys.*

object SegmentPrinter extends Printer[Segment]:
  override def apply[A](segment: Segment[A], a: A): String = segment match
    case Segment.Static(name, _) => name
    case Segment.Parameter(name, codec, metadata) =>
      HttpParameterPrinter(
        explode = metadata.get(explode).getOrElse(false),
        style = metadata.get(style).collect { case style: Header.Style => style }.getOrElse(Header.Style.Simple)
      )(name, codec = codec.value, a)
    case Segment.Modify(self, _, g) => apply(self, g(a))
