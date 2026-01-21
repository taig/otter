package io.taig.otter.codec

import cats.Applicative
import io.taig.otter.z
import io.taig.otter.Json
import io.taig.otter.Typescript
import cats.syntax.all.*

final class JsonZodFTypescriptExpressionRenderer[F[_]: Applicative](
    renderer: Renderer[Json.Read, F[Typescript.Expression]]
) extends Renderer[Json.Read, F[Typescript.Expression]]:
  override def render[A](json: Json.Read[A]): F[Typescript.Expression] = json match
    case json: Json.Constant.Read[A] =>
      z(
        Typescript.Expression.Call(
          name = "literal",
          arguments = List(json.encode(JsonZodPrimitiveTypescriptExpressionEncoder))
        )
      ).pure
    case json: Json.Collection.Read[A] =>
      renderer
        .render(json.schema.value)
        .map(expression => z(Typescript.Expression.Call(name = "array", arguments = List(expression))))
    case json: Json.Dictionary.Read[A] =>
      renderer
        .render(json.schema.value)
        .map: expression =>
          z(
            Typescript.Expression.Call(
              name = "record",
              arguments = List(Typescript.Expression.Symbol("string"), expression)
            )
          )

    case json: Json.Enumeration.Read[A] =>
      z(
        Typescript.Expression.Call(
          name = "enum",
          arguments = List(
            Typescript.Expression.Array(
              elements = json.encode(JsonZodPrimitiveTypescriptExpressionEncoder).toList
            )
          )
        )
      ).pure
    case json: Json.Optional.Read[A] =>
      renderer
        .render(json.schema.value)
        .map(expression => z(Typescript.Expression.Call(name = "nullable", arguments = List(expression))))
    case _: Json.Primitive.Boolean.Read[A] =>
      z(Typescript.Expression.Call(name = "boolean", arguments = Nil)).pure
    case _: Json.Primitive.Coerce.Boolean.Read[A] =>
      z(
        Typescript.Expression
          .Member(
            namespace = "coerce",
            property = Typescript.Expression.Call(name = "boolean", arguments = Nil)
          )
      ).pure
    case _: Json.Primitive.Coerce.Number.Read[A] =>
      z(
        Typescript.Expression
          .Member(
            namespace = "coerce",
            property = Typescript.Expression.Call(name = "number", arguments = Nil)
          )
      ).pure
    case _: Json.Primitive.Coerce.Text.Read[A] =>
      z(
        Typescript.Expression
          .Member(
            namespace = "coerce",
            property = Typescript.Expression.Call(name = "string", arguments = Nil)
          )
      ).pure
    case _: Json.Primitive.Number.Read[A] =>
      z(Typescript.Expression.Call(name = "number", arguments = Nil)).pure
    case _: Json.Primitive.Text.Read[A] =>
      z(Typescript.Expression.Call(name = "string", arguments = Nil)).pure
    case json: Json.Record.Read[A] =>
      json.fields
        .map(_.value)
        .traverse: field =>
          renderer
            .render(field.schema.value)
            .map: expression =>
              if field.isOptional
              then z(Typescript.Expression.Call(name = "optional", arguments = List(expression)))
              else expression
            .map(field.name -> _)
        .map: fields =>
          z(
            Typescript.Expression.Call(
              name = "object",
              arguments = List(Typescript.Expression.Object(fields = fields.toList))
            )
          )
    case json: Json.Tuple.Read[A] =>
      json.schemas
        .map(_.value)
        .traverse(renderer.render)
        .map(expressions => Typescript.Expression.Array(elements = expressions.toList))
        .map: expression =>
          z(Typescript.Expression.Call(name = "tuple", arguments = List(expression)))
    case json: Json.Union.Read[A] =>
      json.branches
        .map(_.value.schema.value)
        .traverse(renderer.render)
        .map(expressions => Typescript.Expression.Array(elements = expressions.toList))
        .map: expression =>
          z(Typescript.Expression.Call(name = "union", arguments = List(expression)))
