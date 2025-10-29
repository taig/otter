package io.taig.otter.codec

import cats.Applicative
import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.ZodTypescriptExpression
import io.taig.otter.syntax.CollectionSyntax.*

final class JsonZodInlineRenderer[F[_]: Applicative](renderer: Renderer[Json, F[ZodTypescriptExpression]])
    extends Renderer[Json, F[String]]:
  override def render[A](json: Json[A]): F[String] = ???
  // json match
  //   case json @ Json.Coerce(_)     => JsonCoerceZodRenderer.render(json).pure
  //   case json @ Json.Constant(_)   => s"z.literal(${json.encode(JsonPrimitiveZodPrinter)})".pure
  //   case json @ Json.Collection(_) =>
  //     renderer.render(json.schema.value).map(expression => s"z.array($expression)")
  //   case json @ Json.Dictionary(_) =>
  //     renderer.render(json.schema.value).map(expression => s"z.record(z.string(), $expression)")
  //   case json @ Json.Enumeration(annotation) =>
  //     s"z.enum(${json.encode(JsonPrimitiveZodPrinter).mkString_("[", ", ", "]")})".pure
  //   case Json.Primitive.Boolean(_) => "z.boolean()".pure
  //   case Json.Primitive.Number(_)  => "z.number()".pure
  //   case Json.Primitive.String(_)  => "z.string()".pure
  //   case json @ Json.Nullable(_)   =>
  //     renderer.render(json.schema.value).map(expression => s"z.nullable($expression)")
  //   case json @ Json.Record(_) =>
  //     json.fields
  //       .map(_.value)
  //       .traverse: field =>
  //         renderer
  //           .render(field.schema.value)
  //           .map: expression =>
  //             val value = if field.isOptional then s"z.optional($expression)" else expression
  //             s""""${field.name}": $value"""
  //       .map(fields => s"z.object(${fields.mkString_("{\n", ",\n", "\n}")})")
  //   case json @ Json.Tuple(_) =>
  //     json.schemas
  //       .map(_.value)
  //       .traverse(renderer.render)
  //       .map(expressions => s"z.tuple(${expressions.mkString_("[", ", ", "]")})")
  //   case json @ Json.Union(_) =>
  //     json.schemas
  //       .map(_.value)
  //       .traverse(renderer.render)
  //       .map(expressions => expressions.mkString_("z.union([", ", ", "])"))
