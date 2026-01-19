package io.taig.otter.codec

import cats.Applicative
import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.JsonZodExpression

final class JsonZodInlineRenderer[F[_]: Applicative](renderer: Renderer[Json.Read, F[JsonZodExpression]])
    extends Renderer[Json.Read, F[String]]:
  override def render[A](json: Json.Read[A]): F[String] = json match
    case json: Json.Constant.Read[A]   => s"z.literal(${json.encode(JsonZodPrimitivePrinter)})".pure
    case json: Json.Collection.Read[A] =>
      renderer.render(json.schema.value).map(expression => s"z.array($expression)")
    case json: Json.Dictionary.Read[A] =>
      renderer.render(json.schema.value).map(expression => s"z.record(z.string(), $expression)")
    case json: Json.Enumeration.Read[A] =>
      s"z.enum(${json.encode(JsonZodPrimitivePrinter).mkString_("[", ", ", "]")})".pure
    case json: Json.Optional.Read[A] =>
      renderer.render(json.schema.value).map(expression => s"z.nullable($expression)")
    case _: Json.Primitive.Boolean.Read[A]        => "z.boolean()".pure
    case _: Json.Primitive.Coerce.Boolean.Read[A] => "z.coerce.boolean()".pure
    case _: Json.Primitive.Coerce.Number.Read[A]  => "z.coerce.number()".pure
    case _: Json.Primitive.Coerce.Text.Read[A]    => "z.coerce.string()".pure
    case _: Json.Primitive.Number.Read[A]         => "z.number()".pure
    case _: Json.Primitive.Text.Read[A]           => "z.string()".pure
    case json: Json.Record.Read[A]                =>
      json.fields
        .map(_.value)
        .traverse: field =>
          renderer
            .render(field.schema.value)
            .map: expression =>
              val value = if field.isOptional then s"z.optional($expression)" else expression
              s""""${field.name}": $value"""
        .map(fields => s"z.object(${fields.mkString_("{\n", ",\n", "\n}")})")
    case json: Json.Tuple.Read[A] =>
      json.schemas
        .map(_.value)
        .traverse(renderer.render)
        .map(expressions => s"z.tuple(${expressions.mkString_("[", ", ", "]")})")
    case json: Json.Union.Read[A] =>
      json.branches
        .map(_.value.schema.value)
        .traverse(renderer.render)
        .map(_.mkString_("z.union([", ", ", "])"))
