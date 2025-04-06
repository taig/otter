package io.taig.otter.http

import io.taig.otter.*
import scala.annotation.tailrec
import io.taig.otter.Enumeration.Modify
import io.taig.otter.Enumeration.Root

object HeaderCodecPrinter extends Printer[Header.Codec]:
  override def apply[A](codec: Header.Codec[A], a: A): String = codec match
    case Header.Codec.Constant(self)    => apply(codec = self, a)
    case Header.Codec.Enumeration(self) => apply(codec = self, a)
    case Header.Codec.Primitive(self)   => PrimitivePrinter.Unquoted(codec = self, a)
    case Header.Codec.Union(self)       => apply(codec = self, a)

  @tailrec
  def apply[A](codec: Constant[Header.Codec.Primitive, A], a: A): String = codec match
    case Constant.Modify(self, _, g) => apply(codec = self, g(a))
    case Constant.Root(reference, _) => apply(codec = reference.self.value, reference.value)

  @tailrec
  def apply[A](codec: Enumeration[Header.Codec.Primitive, A], a: A): String = codec match
    case Enumeration.Modify(self, _, g)      => apply(codec = self, g(a))
    case Enumeration.Root(codec, mapping, _) => apply(codec = codec.value, mapping(a))

  def apply[A](codec: Union.Untagged[Header.Codec, A], a: A): String = codec match
    case Union.Untagged.Branch(_, codec, _)    => apply(codec = codec.value, a)
    case Union.Untagged.Modify(self, _, g)     => apply(codec = self, g(a))
    case Union.Untagged.OrElse(left, right, _) => a.fold(apply(codec = left, _), apply(codec = right, _))
