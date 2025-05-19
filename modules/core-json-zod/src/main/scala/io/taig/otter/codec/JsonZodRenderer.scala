package io.taig.otter.codec

import cats.data.State
import cats.syntax.all.*
import io.taig.otter.codec.Renderer
import io.taig.otter.Json
import io.taig.otter.ZodState
import io.taig.otter.ZodExpression
import cats.data.Chain
import io.taig.otter.indent

object JsonZodRenderer extends Renderer[Json, ZodState[ZodExpression]]:
  val collection = CollectionZodRenderer(renderer = this)
  val record = RecordZodRenderer(
    renderer = FieldZodRenderer(key = KeyPrinter.Quoted, value = this)
      .mapK[Json.Field]([A] => (field: Json.Field[A]) => field.self)
  )

  override def render[A](schema: Json[A]): ZodState[ZodExpression] =
    NamespaceZodRenderer(renderer = ZodRenderer(renderer = Raw)).render(schema)

  object Raw extends Renderer[Json, ZodState[String]]:
    override def render[A](schema: Json[A]): ZodState[String] = schema match
      case Json.Collection(self) => collection.render(schema = self)
      case Json.Primitive(self)  => State.pure(PrimitiveZodRenderer.render(schema = self))
      case Json.Record(self)     => record.render(schema = self)

//   override def apply[A](codec: Json[A]): ZodState[ZodExpression] = NamespaceZodRenderer(renderer = Raw)(codec)

//   object Raw extends Renderer[Json, ZodState[String]]:
//     override def apply[T](codec: Json[T]): ZodState[String] = codec match
//       case Json.Constant(self)    => apply(codec = self)
//       case Json.Collection(self)  => apply(codec = self)
//       case Json.Dictionary(self)  => apply(codec = self)
//       case Json.Enumeration(self) => State.pure(apply(codec = self))
//       case Json.Nullable(self)    => apply(codec = self)
//       case Json.Primitive(self)   => State.pure(PrimitiveZodRenderer(codec = self))
//       case Json.Record(self) =>
//         apply(codec = self).map: fields =>
//           show"""z.object({
//                 |${fields.map((key, value) => indent(show"\"$key\": $value")).mkString_(",\n")}
//                 |})""".stripMargin
//       case Json.Tuple(self) => apply(codec = self)
//       // case Json.Union(self) => apply(codec = self)

//     def apply(codec: Constant[Json, ?]): ZodState[String] =
//       apply(reference = codec.codec)
//         .map(value => State.pure(s"z.literal($value)"))
//         // TODO how to handle this unrepresentable edge case?
//         .getOrElse(self.apply(codec = codec.codec.self.value).map(_.show))

//     def apply(codec: Dictionary[Json.Key, Json, ?]): ZodState[String] = codec match
//       case Dictionary.Root(key, value, _, _, _) =>
//         (JsonKeyZodRenderer(codec = key.value), apply(codec = value.value))
//           .mapN((key, value) => show"""z.record($key, $value)""")
//       case Dictionary.Modify(self, _, _) => apply(codec = self)

//     def apply(codec: Enumeration[Json.Primitive, ?]): String =
//       EnumerationZodRenderer(printer = JsonPrimitivePrinter)(codec)

//     def apply(codec: Nullable[Json, ?]): ZodState[String] =
//       codec.codec
//         .map(_.value)
//         .fold(State.pure("z.void()"))(self.apply(_).map(expression => show"z.nullable(${expression})"))

//     def apply(codec: Tuple[Json, ?]): ZodState[String] = codec.codecs
//       .map(_.value)
//       .traverse(self.apply)
//       .map: values =>
//         s"""z.tuple([
//            |${values.map(value => show"  $value").mkString_(",\n")}
//            |])""".stripMargin

//     // def apply(codec: Union[Json, ?]): ZodState[String] =
//     //   val discriminator = codec match
//     //     case codec: Union.Untagged[Json, ?] => none
//     //     case codec: Union.Tagged[Json, ?]   => codec.discriminator.some

//     //   UnionZodRenderer[Json](
//     //     render = [A] => (name: String, codec: Json[A]) => Raw(name, codec, discriminator)
//     //   )(codec)

//     def apply[A](reference: Reference.Constant[Json, A]): Option[String] = reference.self.value match
//       case Json.Primitive(self) => PrimitivePrinter.Quoted(codec = self, reference.value).some
//       case _                    => none

//     def apply(name: String, codec: Json[?], discriminator: Option[Discriminator]): ZodState[ZodExpression] =
//       discriminator match
//         case Some(Discriminator.Keyed) =>
//           self
//             .apply(codec)
//             .map: expression =>
//               ZodExpression.Inline:
//                 show"""z.object({
//                       |${indent(show""""$name": $expression""")}
//                       |})""".stripMargin
//         case Some(Discriminator.Merged(identifier)) =>
//           self
//             .apply(codec)
//             .map: expression =>
//               ZodExpression.Inline:
//                 show"""$expression.merge(z.object({ "$identifier": z.literal("$name") }))"""
//         case Some(Discriminator.Explicit(identifier, value)) =>
//           self
//             .apply(codec)
//             .map: expression =>
//               ZodExpression.Inline:
//                 show"""z.object({
//                       |  "$identifier": z.literal("$name"),
//                       |  "$value": $expression
//                       |})""".stripMargin
//         case None => self.apply(codec)

// object JsonZodRenderer:
//   def apply(): Renderer[Json, ZodState[ZodExpression]] = new JsonZodRenderer()
