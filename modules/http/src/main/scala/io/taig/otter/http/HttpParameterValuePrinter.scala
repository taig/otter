package io.taig.otter.http

import io.taig.otter.*

import scala.annotation.tailrec

object HttpParameterValuePrinter extends Printer[Http.Parameter.Value]:
  override def apply[A](codec: Http.Parameter.Value[A], a: A): String = codec match
    case Http.Parameter.Value.Constant(self)    => apply(codec = self, a)
    case Http.Parameter.Value.Enumeration(self) => apply(codec = self, a)
    case Http.Parameter.Value.Primitive(self)   => PrimitivePrinter.Unquoted(codec = self, a)
    // case Http.Parameter.Value.Union(self)       => apply(codec = self, a)

  @tailrec
  def apply[A](codec: Constant[Http.Parameter.Value.Primitive, A], a: A): String = codec match
    case Constant.Modify(self, _, g)    => apply(codec = self, g(a))
    case Constant.Root(reference, _, _) => apply(codec = reference.self.value, reference.value)

  @tailrec
  def apply[A](codec: Enumeration[Http.Parameter.Value.Primitive, A], a: A): String = codec match
    case Enumeration.Modify(self, _, g)      => apply(codec = self, g(a))
    case Enumeration.Root(codec, mapping, _) => apply(codec = codec.value, mapping(a))

  // def apply[A](codec: Union.Untagged[Http.Parameter.Value, A], a: A): String = codec match
  //   case Union.Untagged.Branch(_, codec, _)    => apply(codec = codec.value, a)
  //   case Union.Untagged.Modify(self, _, g)     => apply(codec = self, g(a))
  //   case Union.Untagged.OrElse(left, right, _) => a.fold(apply(codec = left, _), apply(codec = right, _))
