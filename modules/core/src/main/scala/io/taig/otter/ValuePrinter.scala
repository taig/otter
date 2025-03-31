package io.taig.otter

import scala.annotation.tailrec

object ValuePrinter:
  def apply[A](value: Value[A], a: A): String = value.codec match
    case codec: Constant[Value, ?]       => apply(codec, a)
    case codec: Enumeration[Value, ?]    => apply(codec, a)
    case codec: Primitive[?]             => apply(codec, a)
    case codec: Union.Untagged[Value, ?] => ???

  @tailrec
  def apply[A](codec: Constant[Value, A], a: A): String = codec match
    case Constant.Root(codec, reference, _) => ValuePrinter(value = codec.value, reference)
    case Constant.Modify(self, _, g)        => apply(codec = self, g(a))

  @tailrec
  def apply[A](codec: Enumeration[Value, A], a: A): String = codec match
    case Enumeration.Modify(self, _, g)      => apply(codec = self, g(a))
    case Enumeration.Root(codec, mapping, _) => apply(value = codec.value, mapping(a))

  @tailrec
  def apply[A](codec: Primitive[A], value: A): String = codec match
    case _: Primitive.String                        => value
    case _: Primitive.BigDecimal                    => value.toPlainString
    case _: Primitive.BigInteger                    => value.toString
    case _: Primitive.Float                         => String.valueOf(value)
    case _: Primitive.Double                        => String.valueOf(value)
    case _: Primitive.Int                           => String.valueOf(value)
    case _: Primitive.Long                          => String.valueOf(value)
    case _: Primitive.Boolean                       => String.valueOf(value)
    case Primitive.Modify(self, _, g)               => apply(codec = self, g(value))
    case Primitive.Parser(_, _, encode, _, _, _, _) => encode(value)

  def apply[A](codec: Union.Untagged[Value, A], a: A): String = codec match
    case Union.Untagged.Modify(self, _, g)     => apply(codec = self, g(a))
    case Union.Untagged.Root(branch, _)        => apply(branch, a)
    case Union.Untagged.OrElse(left, right, _) => a.fold(apply(codec = left, _), apply(codec = right, _))

  @tailrec
  def apply[A](branch: Branch[Value, A], a: A): String = branch match
    case Branch.Modify(self, f, g) => apply(branch = self, g(a))
    case Branch.Root(_, codec, _)  => apply(value = codec.value, a)
