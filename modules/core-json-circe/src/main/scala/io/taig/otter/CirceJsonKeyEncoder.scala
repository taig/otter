package io.taig.otter

import scala.annotation.tailrec

object CirceJsonKeyEncoder:
  def apply[A](codec: Json.Key[A], a: A): String = codec.self match
    case codec: Constant[Json.Key, ?]       => apply(codec, a)
    case codec: Enumeration[?]              => apply(codec, a)
    case codec: Primitive[?]                => apply(codec, a)
    case codec: Union.Untagged[Json.Key, ?] => apply(codec, a)

  @tailrec
  def apply[A](codec: Constant[Json.Key, A], a: A): String = codec match
    case Constant.Root(codec, reference, _) => apply(codec = codec.value, reference)
    case Constant.Modify(self, _, g)        => apply(codec = self, g(a))

  @tailrec
  def apply[A](codec: Enumeration[A], a: A): String = codec match
    case Enumeration.Modify(self, _, g)      => apply(codec = self, g(a))
    case Enumeration.Root(codec, mapping, _) => apply(codec = codec, mapping(a))

  @tailrec
  def apply[A](codec: Primitive[A], a: A): String = codec match
    case _: Primitive.String                        => a
    case _: Primitive.BigDecimal                    => a.toPlainString
    case _: Primitive.BigInteger                    => a.toString
    case _: Primitive.Float                         => String.valueOf(a)
    case _: Primitive.Double                        => String.valueOf(a)
    case _: Primitive.Int                           => String.valueOf(a)
    case _: Primitive.Long                          => String.valueOf(a)
    case _: Primitive.Boolean                       => String.valueOf(a)
    case Primitive.Modify(self, _, g)               => apply(codec = self, g(a))
    case Primitive.Parser(_, _, encode, _, _, _, _) => encode(a)

  def apply[A](codec: Union.Untagged[Json.Key, A], a: A): String = codec match
    case Union.Untagged.Modify(self, _, g)     => apply(codec = self, g(a))
    case Union.Untagged.Root(branch, _)        => apply(branch, a)
    case Union.Untagged.OrElse(left, right, _) => a.fold(apply(codec = left, _), apply(codec = right, _))

  @tailrec
  def apply[A](branch: Branch[Json.Key, A], a: A): String = branch match
    case Branch.Modify(self, f, g) => apply(branch = self, g(a))
    case Branch.Root(_, codec, _)  => apply(codec = codec.value, a)
