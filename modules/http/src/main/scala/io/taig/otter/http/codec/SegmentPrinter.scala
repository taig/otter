package io.taig.otter.http.codec

import io.taig.otter.codec.Printer
import io.taig.otter.http.Segment

final class SegmentPrinter[F[_]](printer: Printer[F]) extends Printer[Segment.Write[F, *]]:
  override def encode[A](segment: Segment.Write[F, A], a: A): String = segment match
    case Segment.Dynamic.Modify(self, _, f)    => encode(segment = self, f(a))
    case Segment.Dynamic.Root(name, schema)    => printer.encode(schema.value, a)
    case Segment.Dynamic.Write.Modify(self, f) => encode(segment = self, f(a))
    case Segment.Static.Modify(self, _, f)     => encode(segment = self, f(a))
    case Segment.Static.Root(name)             => name
    case Segment.Static.Write.Modify(self, f)  => encode(segment = self, f(a))
