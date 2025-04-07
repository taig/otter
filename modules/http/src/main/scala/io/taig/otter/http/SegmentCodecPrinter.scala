package io.taig.otter.http

import io.taig.otter.*
import scala.annotation.tailrec
import io.taig.otter.Enumeration.Modify
import io.taig.otter.Enumeration.Root

object SegmentCodecPrinter extends Printer[Segment.Codec]:
  override def apply[A](codec: Segment.Codec[A], a: A): String = codec match
    case Segment.Codec.Constant(self)    => apply(codec = self, a)
    case Segment.Codec.Enumeration(self) => apply(codec = self, a)
    case Segment.Codec.Primitive(self)   => PrimitivePrinter.Unquoted(codec = self, a)
    case Segment.Codec.Union(self)       => apply(codec = self, a)

  @tailrec
  def apply[A](codec: Constant[Segment.Codec.Primitive, A], a: A): String = codec match
    case Constant.Modify(self, _, g) => apply(codec = self, g(a))
    case Constant.Root(reference, _) => apply(codec = reference.self.value, reference.value)

  @tailrec
  def apply[A](codec: Enumeration[Segment.Codec.Primitive, A], a: A): String = codec match
    case Enumeration.Modify(self, _, g)      => apply(codec = self, g(a))
    case Enumeration.Root(codec, mapping, _) => apply(codec = codec.value, mapping(a))

  def apply[A](codec: Union.Untagged[Segment.Codec, A], a: A): String = codec match
    case Union.Untagged.Branch(_, codec, _)    => apply(codec = codec.value, a)
    case Union.Untagged.Modify(self, _, g)     => apply(codec = self, g(a))
    case Union.Untagged.OrElse(left, right, _) => a.fold(apply(codec = left, _), apply(codec = right, _))
