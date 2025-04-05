package io.taig.otter

import scala.annotation.tailrec

object CirceJsonKeyEncoder extends Encoder[Json.Key, String]:
  def apply[A](codec: Json.Key[A], a: A): String = codec.value match
    case codec: Constant[Json.Key, A]       => apply(codec, a)
    case codec: Enumeration[Json.Key, A]    => apply(codec, a)
    case codec: Primitive.String[A]         => apply(codec, a)
    case codec: Union.Untagged[Json.Key, A] => apply(codec, a)

  def apply[A](codec: Constant[Json.Key, A], a: A): String = apply(codec = codec.codec)

  @tailrec
  def apply[A](codec: Enumeration[Json.Key, A], a: A): String = codec match
    case Enumeration.Modify(self, _, g)      => apply(codec = self, g(a))
    case Enumeration.Root(codec, mapping, _) => apply(codec = codec.value, mapping(a))

  @tailrec
  def apply[A](codec: Primitive.String[A], a: A): String = codec match
    case _: Primitive.String.Text                          => a
    case Primitive.String.Modify(self, _, g)               => apply(codec = self, g(a))
    case Primitive.String.Parser(_, _, encode, _, _, _, _) => encode(a)

  def apply[A](codec: Union.Untagged[Json.Key, A], a: A): String = codec match
    case Union.Untagged.Branch(name, codec, _) => apply(codec = codec.value, a)
    case Union.Untagged.Modify(self, _, g)     => apply(codec = self, g(a))
    case Union.Untagged.OrElse(left, right, _) => a.fold(apply(left, _), apply(right, _))

  def apply[A](codec: Reference.Constant[Json.Key, A]): String =
    apply(codec = codec.self.value, codec.value)
