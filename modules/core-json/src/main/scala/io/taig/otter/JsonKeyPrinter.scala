package io.taig.otter

import io.taig.otter.Json.Key
import io.taig.otter.Union.Untagged.OrElse
import io.taig.otter.Union.Untagged.Branch
import io.taig.otter.Union.Untagged.Modify
import io.taig.otter.Primitive.String.Text

object JsonKeyPrinter extends Printer[Json.Key]:
  override def apply[A](codec: Json.Key[A], a: A): String = codec match
    case Json.Key.Constant(value)  => ReferenceConstantPrinter(printer = JsonKeyPrinter)(codec = value.codec)
    case Json.Key.Primitive(value) => apply(codec = value, a)
    case Json.Key.Union(value)     => apply(codec = value, a)

  def apply[A](codec: Primitive.String[A], a: A): String = codec match
    case Primitive.String.Parser(_, _, encode, _, _, _, _) => apply(encode(a))
    case Primitive.String.Text(_, _, _, _)                 => apply(a)
    case Primitive.String.Modify(self, _, g)               => apply(codec = self, g(a))

  def apply(value: String): String = "\"" + value.replace("\"", "\\\"") + "\""

  def apply[A](codec: Union.Untagged[Json.Key, A], a: A): String = codec match
    case Union.Untagged.OrElse(left, right, _) => a.fold(apply(left, _), apply(right, _))
    case Union.Untagged.Branch(_, codec, _)    => apply(codec = codec.value, a)
    case Union.Untagged.Modify(self, _, g)     => apply(codec = self, g(a))
