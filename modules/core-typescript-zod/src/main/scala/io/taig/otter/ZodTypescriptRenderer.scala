package io.taig.otter

import io.taig.otter.Keys.*
import scala.collection.immutable.ListMap
import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Enumeration.Modify
import io.taig.otter.Enumeration.Root

object ZodTypescriptRenderer:
  def apply(codec: Typescript[?]): State[ListMap[Const, String], Expression] =
    codec.self.metadata.get(typescript) match
      case Some(typescript) => State.pure(Expression.Inline(typescript))
      case None =>
        codec.self match
          case codec: Collection[Typescript, ?] => ???
          case codec: Constant[Primitive, ?]    => State.pure(Expression.Inline(apply(codec)))
          case codec: Enumeration[?]            => State.pure(Expression.Inline(apply(codec)))
          case codec: Primitive[?]              => State.pure(Expression.Inline(apply(codec)))

  def apply(codec: Constant[Primitive, ?]): String = s"z.literal(${apply(codec.codec.value)})"

  def apply(codec: Enumeration[?]): String = codec match
    case Enumeration.Modify(self, _, _) => apply(codec = self)
    case self @ Enumeration.Root(codec, mapping, _) =>
      val values = self.values.map(a => PrimitivePrinter(codec, mapping(a))).mkString_(" | ")
      s"z.enum([$values])"

  def apply(codec: Primitive[?]): String = codec match
    case _: Primitive.Boolean                         => "z.boolean()"
    case _: Primitive.String | _: Primitive.Parser[?] => "z.string()"
    case _: Primitive.Double | _: Primitive.Float | _: Primitive.Int | _: Primitive.Long | _: Primitive.BigDecimal |
        _: Primitive.BigInteger =>
      "z.number()"
    case Primitive.Modify(self, _, _) => apply(codec = self)
