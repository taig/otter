package io.taig.otter.http

import io.taig.otter.*
import cats.syntax.all.*
import scala.annotation.tailrec
import io.taig.otter.Record.Empty
import io.taig.otter.Record.Field
import io.taig.otter.Record.Modify
import io.taig.otter.Record.Zip
import cats.data.Chain

// TODO escape
object SegmentCodecObjectPrinter:
  def apply[A](codec: Segment.Codec.Object[A], a: A): Chain[(String, String)] = codec match
    case Segment.Codec.Object.Dictionary(self) => Chain.fromSeq(apply(codec = self, a))
    case Segment.Codec.Object.Record(self)     => apply(codec = self, a)

  @tailrec
  def apply[A](codec: Dictionary[Segment.Codec, Segment.Codec, A], a: A): List[(String, String)] = codec match
    case Dictionary.Root(key, codec, _, _, _) =>
      a.map: (name, value) =>
        (SegmentCodecPrinter(codec = key.value, name), SegmentCodecPrinter(codec = codec.value, value))
    case Dictionary.Modify(self, f, g) => apply(codec = self, g(a))

  def apply[A](codec: Record[Segment.Codec, Segment.Codec, A], a: A): Chain[(String, String)] = codec match
    case Record.Empty(_) => Chain.empty
    case Record.Field(key, codec, _) =>
      val name = SegmentCodecPrinter(codec = key.self.value, key.value)
      val value = SegmentCodecPrinter(codec = codec.value, a)
      Chain.one((name, value))
    case Record.Modify(self, f, g)         => apply(codec = self, g(a))
    case Record.Optional(self)             => a.fold(Chain.empty)(apply(codec = self, _))
    case Record.Zip(left, right, metadata) => apply(codec = left, a._1) ++ apply(codec = right, a._2)
