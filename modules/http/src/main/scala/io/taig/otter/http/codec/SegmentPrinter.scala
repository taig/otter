package io.taig.otter.http.codec

import io.taig.otter.codec.Printer
import io.taig.otter.http.Segment

object SegmentPrinter extends Printer[Segment.Write]:
  override def encode[A](segment: Segment.Write[A], a: A): String = segment match
    case Segment.Dynamic.Modify(self, _, f)       => encode(segment = self, f(a))
    case Segment.Dynamic.Root(name, schema)       => SegmentParameterPrinter.encode(parameter = schema.value, a)
    case Segment.Dynamic.Write.Modify(self, f)    => encode(segment = self, f(a))
    case Segment.Dynamic.Write.Root(name, schema) => SegmentParameterPrinter.encode(parameter = schema.value, a)
    case Segment.Static.Modify(self, _, f)        => encode(segment = self, f(a))
    case Segment.Static.Root(name)                => name
    case Segment.Static.Write.Modify(self, f)     => encode(segment = self, f(a))
