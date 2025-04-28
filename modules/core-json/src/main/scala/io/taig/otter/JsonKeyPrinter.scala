package io.taig.otter

import scala.annotation.tailrec

object JsonKeyPrinter extends Printer[Json.Key]:
  override def apply[A](codec: Json.Key[A], a: A): String = codec match
    case Json.Key.Constant(self)    => apply(codec = self, a)
    case Json.Key.Enumeration(self) => apply(codec = self, a)
    case Json.Key.Primitive(self)   => PrimitivePrinter.Unquoted(codec = self, a)
    case Json.Key.Union(self)       => apply(codec = self, a)

  @tailrec
  def apply[A](codec: Constant[Json.Key.Primitive, A], a: A): String = codec match
    case Constant.Modify(self, _, g) => apply(codec = self, g(a))
    case Constant.Root(codec, _, _)  => apply(codec = codec.self.value, codec.value)

  @tailrec
  def apply[A](codec: Enumeration[Json.Key.Primitive, A], a: A): String = codec match
    case Enumeration.Modify(self, _, g)      => apply(codec = self, g(a))
    case Enumeration.Root(codec, mapping, _) => apply(codec = codec.value, mapping(a))

  def apply[A](codec: Union.Untagged[Json.Key, A], a: A): String = codec match
    case Union.Untagged.Branch(_, codec, _)    => apply(codec = codec.value, a)
    case Union.Untagged.OrElse(left, right, _) => a.fold(apply(left, _), apply(right, _))
    case Union.Untagged.Modify(self, _, g)     => apply(codec = self, g(a))
