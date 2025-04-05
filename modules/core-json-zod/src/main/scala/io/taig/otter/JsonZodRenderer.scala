package io.taig.otter

import cats.data.Chain
import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Keys.*

import scala.collection.immutable.ListMap

final class JsonZodRenderer extends Renderer[Json, ZodState[Expression]]:
  override def apply[A](codec: Json[A]): ZodState[Expression] = ???
  // State: state =>
  //   codec.metadata.get(name) match
  //     case Some(name) =>
  //       val reference = Const(namespace = codec.metadata.get(namespace), name)
  //       val (update, result) = force(codec).run(initial = state).value
  //       (update.updatedWith(reference)(_ => Some(result)), Expression.Referenced(reference, result))
  //     case None => force(codec).run(initial = state).value.map(Expression.Inline.apply)

  // def force(codec: Json[?]): State[ListMap[Const, String], String] = State: state =>
  //   codec.metadata.get(typescript) match
  //     case Some(typescript) => (state, typescript)
  //     case None =>
  //       codec match
  //         case codec: Json.Collection[?] => apply(codec).run(initial = state).value
  //         case codec: Json.Constant[?]   => (state, apply(codec))
  //         case codec: Json.Dictionary[?] => apply(codec).run(initial = state).value
  //         case codec: Json.Optional[?]   => apply(codec).run(initial = state).value
  //         case codec: Json.Primitive[?]  => (state, apply(codec))
  //         case codec: Json.Record[?]     => apply(codec).run(initial = state).value
  //         case codec: Json.Tuple[?]      => apply(codec).run(initial = state).value
  //         case codec: Json.Union[?]      => apply(codec).run(initial = state).value

  // def apply(codec: Json.Collection[?]): State[ListMap[Const, String], String] = apply(codec = codec.value)

  // def apply(codec: Collection[Json, ?]): State[ListMap[Const, String], String] =
  //   apply(codec = codec.codec.value).map(expression => show"z.array($expression)")

  // def apply(codec: Json.Constant[?]): String =
  //   // s"z.literal(${apply(codec = codec.value)})"
  //   ???

  // def apply(codec: Json.Dictionary[?]): State[ListMap[Const, String], String] = apply(codec = codec.value)

  // def apply(codec: Dictionary[Json.Key, Json, ?]): State[ListMap[Const, String], String] = codec match
  //   case Dictionary.Root(key, value, _, _, _) =>
  //     (JsonKeyZodRenderer(codec = key.value), apply(codec = value.value))
  //       .mapN((key, value) => show"""z.record($key, $value)""")
  //   case Dictionary.Modify(self, _, _) => apply(codec = self)

  // // def apply(codec: Json.Enumeration[?]): String = apply(codec = codec.value)

  // // @tailrec
  // // def apply(codec: Enumeration[Json.Primitive, ?]): String = codec match
  // //   case Enumeration.Modify(self, _, _) => apply(codec = self)
  // //   case self @ Enumeration.Root(codec, mapping, _) =>
  // //     val values = self.values.map(a => PrimitivePrinter(codec = codec.value.value, mapping(a))).mkString_(", ")
  // //     s"z.enum([$values])"

  // def apply(codec: Json.Optional[?]): State[ListMap[Const, String], String] = apply(codec = codec.value)

  // def apply(codec: Optional[Json, ?]): State[ListMap[Const, String], String] =
  //   apply(codec.codec.value).map(expression => show"z.nullable(${expression})")

  // def apply(codec: Json.Primitive[?]): String = codec match
  //   case _: Json.Primitive.Boolean[?] => "z.boolean()"
  //   case _: Json.Primitive.String[?]  => "z.string()"
  //   case _: Json.Primitive.Number[?]  => "z.number()"

  // def apply(codec: Json.Record[?]): State[ListMap[Const, String], String] =
  //   apply(codec = codec.value).map: values =>
  //     s"""z.object({
  //        |${values.map((key, value) => indent(show"\"$key\": $value")).mkString_(",\n")}
  //        |})""".stripMargin

  // // TODO figure out a proper way to encode partially optional objects
  // def apply(codec: Record[Json.Key, Json, ?]): State[ListMap[Const, String], Chain[(String, Expression)]] = codec match
  //   case Record.Empty(_)             => State.pure(Chain.empty)
  //   case Record.Field(key, value, _) =>
  //     // apply(codec = codec.value).map(value => Chain.one((name, value)))
  //     apply(reference = key)
  //     ???
  //   case Record.Modify(self, _, _) => apply(codec = self)
  //   case Record.Optional(self) =>
  //     apply(codec = self).map: values =>
  //       values.map((key, value) => (key, Expression.Inline(show"z.optional($value)")))
  //   case Record.Zip(left, right, _) => (apply(codec = left), apply(codec = right)).mapN(_ ++ _)

  // def apply(codec: Json.Tuple[?]): State[ListMap[Const, String], String] = codec.value.codecs
  //   .map(_.value)
  //   .traverse(apply)
  //   .map: values =>
  //     s"""z.tuple([
  //        |${values.map(value => show"  $value").mkString_(",\n")}
  //        |])""".stripMargin

  // def apply(codec: Json.Union[?]): State[ListMap[Const, String], String] = codec.value.branches
  //   .traverse((name, reference) => apply(name, reference, discriminator = codec.discriminator))
  //   .map: values =>
  //     s"""z.union([
  //        |${indent(values.map(value => show"$value").mkString_(",\n"))}
  //        |])""".stripMargin

  // def apply(
  //     name: String,
  //     codec: Reference[Json, ?],
  //     discriminator: Option[Discriminator]
  // ): State[ListMap[Const, String], Expression] =
  //   discriminator match
  //     case Some(Discriminator.Keyed) =>
  //       apply(codec = codec.value).map: expression =>
  //         Expression.Inline:
  //           show"""z.object({
  //                 |${indent(show""""$name": $expression""")}
  //                 |})""".stripMargin
  //     case Some(Discriminator.Merged(identifier)) =>
  //       apply(codec = codec.value).map: expression =>
  //         Expression.Inline:
  //           show"""$expression.merge(z.object({ "$identifier": z.literal("$name") }))"""
  //     case Some(Discriminator.Explicit(identifier, value)) =>
  //       apply(codec = codec.value).map: expression =>
  //         Expression.Inline:
  //           show"""z.object({
  //                 |  "$identifier": z.literal("$name"),
  //                 |  "$value": $expression
  //                 |})""".stripMargin
  //     case None => apply(codec = codec.value)

  // def apply[A](reference: Reference.Constant[Json.Key, A]): String =
  //   ??? // PrimitivePrinter(codec = reference.self.value.value, reference.value)

  // def apply[A](codec: Json.Key[A], a: A): String = ???

object JsonZodRenderer:
  def apply(): Renderer[Json, ZodState[Expression]] = new JsonZodRenderer()
