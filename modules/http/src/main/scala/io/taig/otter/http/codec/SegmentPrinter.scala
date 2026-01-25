package io.taig.otter.http.codec

import io.taig.otter.codec.Printer
import io.taig.otter.http.Segment

object SegmentPrinter extends Printer[Segment.Write]:
  override def encode[A](segment: Segment.Write[A], a: A): String = segment match
    case Segment.Parameter.Modify(self, _, f)       => encode(segment = self, f(a))
    case Segment.Parameter.Root(name, schema)       => SegmentValuePrinter.encode(value = schema.value, a)
    case Segment.Parameter.Write.Modify(self, f)    => encode(segment = self, f(a))
    case Segment.Parameter.Write.Root(name, schema) => SegmentValuePrinter.encode(value = schema.value, a)
    case Segment.Static.Modify(self, _, f)          => encode(segment = self, f(a))
    case Segment.Static.Root(name)                  => name
    case Segment.Static.Write.Modify(self, f)       => encode(segment = self, f(a))
