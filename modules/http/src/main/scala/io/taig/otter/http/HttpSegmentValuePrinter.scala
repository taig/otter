package io.taig.otter.http

import io.taig.otter.*

import scala.annotation.tailrec

object HttpSegmentValuePrinter extends Printer[Http.Segment.Value]:
  override def apply[A](codec: Http.Segment.Value[A], a: A): String = codec match
    case Http.Segment.Value.Constant(self)    => apply(codec = self, a)
    case Http.Segment.Value.Enumeration(self) => apply(codec = self, a)
    case Http.Segment.Value.Primitive(self)   => PrimitivePrinter.Unquoted(codec = self, a)
    case Http.Segment.Value.Union(self)       => apply(codec = self, a)

  @tailrec
  def apply[A](codec: Constant[Http.Segment.Value.Primitive, A], a: A): String = codec match
    case Constant.Modify(self, _, g) => apply(codec = self, g(a))
    case Constant.Root(reference, _, _) => apply(codec = reference.self.value, reference.value)

  @tailrec
  def apply[A](codec: Enumeration[Http.Segment.Value.Primitive, A], a: A): String = codec match
    case Enumeration.Modify(self, _, g)      => apply(codec = self, g(a))
    case Enumeration.Root(codec, mapping, _) => apply(codec = codec.value, mapping(a))

  def apply[A](codec: Union.Untagged[Http.Segment.Value, A], a: A): String = codec match
    case Union.Untagged.Branch(_, codec, _)    => apply(codec = codec.value, a)
    case Union.Untagged.Modify(self, _, g)     => apply(codec = self, g(a))
    case Union.Untagged.OrElse(left, right, _) => a.fold(apply(codec = left, _), apply(codec = right, _))
