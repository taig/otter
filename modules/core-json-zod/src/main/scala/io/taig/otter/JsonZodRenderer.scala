package io.taig.otter

import cats.data.State
import io.taig.otter.Keys.*
import scala.collection.immutable.ListMap
import cats.syntax.all.*
import scala.annotation.tailrec
import io.taig.otter.Collection.Linked
import io.taig.otter.Collection.Modify
import io.taig.otter.Dictionary.Root

final class JsonZodRenderer extends Renderer[Json[?], State[ListMap[Const, String], Expression]]:
  override def apply(codec: Json[?]): State[ListMap[Const, String], Expression] = State: state =>
    codec.metadata.get(name) match
      case Some(name) =>
        val reference = Const(namespace = codec.metadata.get(namespace), name)
        val (update, result) = force(codec).run(initial = state).value
        (update.updatedWith(reference)(_ => Some(result)), Expression.Referenced(reference, result))
      case None => force(codec).run(initial = state).value.map(Expression.Inline.apply)

  def force(codec: Json[?]): State[ListMap[Const, String], String] = State: state =>
    codec.metadata.get(typescript) match
      case Some(typescript) => (state, typescript)
      case None =>
        codec match
          case codec: Json.Collection[?]  => apply(codec).run(initial = state).value
          case codec: Json.Constant[?]    => (state, apply(codec))
          case codec: Json.Dictionary[?]  => apply(codec).run(initial = state).value
          case codec: Json.Enumeration[?] => (state, apply(codec))
          case codec: Json.Primitive[?]   => (state, apply(codec))

  def apply(codec: Json.Collection[?]): State[ListMap[Const, String], String] = apply(codec = codec.value)

  def apply(codec: Collection[Json, ?]): State[ListMap[Const, String], String] = codec match
    case Collection.Indexed(codec, _, _, _, _) =>
      apply(codec = codec.value).map(expression => show"z.array($expression)")
    case Collection.Linked(codec, _, _, _, _) =>
      apply(codec = codec.value).map(expression => show"z.array($expression)")
    case Collection.Modify(self, _, _) => apply(codec = self)

  def apply(codec: Json.Constant[?]): String = apply(codec = codec.value)

  def apply(codec: Constant[Json.Primitive, ?]): String = codec match
    case Constant.Modify(self, f, g) => apply(codec = self)
    case Constant.Root(codec, reference, _) =>
      val value = PrimitivePrinter(codec = codec.value.value, reference)
      s"z.literal($value)"

  def apply(codec: Json.Dictionary[?]): State[ListMap[Const, String], String] = apply(codec = codec.value)

  def apply(codec: Dictionary[Json.Key, Json, ?]): State[ListMap[Const, String], String] = codec match
    case Dictionary.Root(key, value, _, _, _) =>
      (apply(codec = key.value: Json[?]), apply(codec = value.value)).mapN: (key, value) =>
        show"""z.record($key, $value)"""
    case Dictionary.Modify(self, _, _) => apply(codec = self)

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

object JsonZodRenderer:
  def apply(): Renderer[Json[?], State[ListMap[Const, String], Expression]] = new JsonZodRenderer()
