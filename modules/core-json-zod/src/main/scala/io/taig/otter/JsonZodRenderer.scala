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
          case Json.Collection(codec)   => ???
          case Json.Constant(codec)     => State.pure(Expression.Inline(apply(codec)))
          case codec: Json.Primitive[?] => State.pure(Expression.Inline(apply(codec)))
        // codec.self match
        //   case codec: Collection[Typescript, ?] => ???
        //   case codec: Constant[Primitive, ?]    => State.pure(Expression.Inline(apply(codec)))
        //   case codec: Enumeration[?]            => State.pure(Expression.Inline(apply(codec)))
        //   case codec: Primitive[?]              => State.pure(Expression.Inline(apply(codec)))

  def apply(codec: Constant[Json.Primitive, ?]): String = s"z.literal(${apply(codec.codec.value)})"

  def apply(codec: Json.Enumeration[?]): String = apply(codec = codec.value)

  @tailrec
  def apply(codec: Enumeration[?]): String = codec match
    case Enumeration.Modify(self, _, _) => apply(codec = self)
    case self @ Enumeration.Root(codec, mapping, _) =>
      val values = self.values.map(a => PrimitivePrinter(codec, mapping(a))).mkString_(" | ")
      s"z.enum([$values])"

  def apply(codec: Json.Primitive[?]): String = codec match
    case _: Json.Primitive.Boolean[?] => "z.boolean()"
    case _: Json.Primitive.String[?]  => "z.string()"
    case _: Json.Primitive.Number[?]  => "z.number()"
