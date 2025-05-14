package io.taig.otter.http

import io.taig.otter.*

import scala.annotation.tailrec
import io.taig.otter.schema.Constant

object HttpHeaderValuePrinter extends Printer[Http.Header.Value]:
  override def apply[A](codec: Http.Header.Value[A], a: A): String = codec match
    case Http.Header.Value.Constant(self)    => apply(codec = self, a)
    case Http.Header.Value.Enumeration(self) => apply(codec = self, a)
    case Http.Header.Value.Primitive(self)   => PrimitivePrinter.Unquoted(codec = self, a)
    // case Http.Header.Value.Union(self)       => apply(codec = self, a)

  @tailrec
  def apply[A](codec: Constant[Http.Header.Value.Primitive, A], a: A): String = codec match
    case Constant.Modify(self, _, g) => apply(codec = self, g(a))
    case Constant.Root(codec, _, _)  => apply(codec = codec.self.value, codec.value)

  @tailrec
  def apply[A](codec: Enumeration[Http.Header.Value.Primitive, A], a: A): String = codec match
    case Enumeration.Modify(self, _, g)      => apply(codec = self, g(a))
    case Enumeration.Root(codec, mapping, _) => apply(codec = codec.value, mapping(a))

  // def apply[A](codec: Union.Untagged[Http.Header.Value, A], a: A): String = codec match
  //   case Union.Untagged.Branch(_, codec, _)    => apply(codec = codec.value, a)
  //   case Union.Untagged.Modify(self, _, g)     => apply(codec = self, g(a))
  //   case Union.Untagged.OrElse(left, right, _) => a.fold(apply(codec = left, _), apply(codec = right, _))
