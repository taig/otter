package io.taig.otter

import cats.data.State
import io.taig.otter.Keys.*
import scala.collection.immutable.ListMap
import cats.syntax.all.*
import scala.annotation.tailrec

object JsonZodRenderer:
  def apply(codec: Json[?]): State[ListMap[Const, String], Expression] =
    codec.metadata.get(typescript) match
      case Some(typescript) => State.pure(Expression.Inline(typescript))
      case None =>
        codec match
          case codec: Json.Constant[?]     => State.pure(Expression.Inline(apply(codec)))
          case codec: Json.Enumeration[?]  => State.pure(Expression.Inline(apply(codec)))
          case codec: Json.Primitive[?] => State.pure(Expression.Inline(apply(codec)))

  def apply(codec: Json.Constant[?]): String = apply(codec = codec.value)

  def apply(codec: Constant[Json.Primitive, ?]): String = codec match
    case Constant.Modify(self, f, g) => apply(codec = self)
    case Constant.Root(codec, reference, _) =>
      val value = PrimitivePrinter(codec = codec.value.value, reference)
      s"z.literal($value)"
  

  def apply(codec: Json.Enumeration[?]): String = apply(codec = codec.value)

  @tailrec
  def apply(codec: Enumeration[Json.Primitive, ?]): String = codec match
    case Enumeration.Modify(self, _, _) => apply(codec = self)
    case self @ Enumeration.Root(codec, mapping, _) =>
      val values = self.values.map(a => PrimitivePrinter(codec = codec.value.value, mapping(a))).mkString_(", ")
      s"z.enum([$values])"

  def apply(codec: Json.Primitive[?]): String = codec match
    case _: Json.Primitive.Boolean[?] => "z.boolean()"
    case _: Json.Primitive.String[?]  => "z.string()"
    case _: Json.Primitive.Number[?]  => "z.number()"
