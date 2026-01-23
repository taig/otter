package io.taig.otter.codec

import cats.Applicative
import io.taig.otter.z
import io.taig.otter.Json
import io.taig.otter.Typescript
import cats.syntax.all.*

final class JsonTypescriptExpressionZodRenderer[F[_]: Applicative](
    renderer: Renderer[Json.Write, F[Typescript.Expression]]
) extends Renderer[Json.Write, F[Typescript.Expression]]:
  override def render[A](json: Json.Write[A]): F[Typescript.Expression] = json match
    case json: Json.Constant.Write[A] =>
      z(
        Typescript.Expression.Call(
          name = "literal",
          arguments = List(json.encode(JsonPrimitiveTypescriptExpressionLiteralEncoder))
        )
      ).pure
    case json: Json.Collection.Write[A] =>
      renderer
        .render(json.schema.value)
        .map(expression => z(Typescript.Expression.Call(name = "array", arguments = List(expression))))
    case json: Json.Dictionary.Write[A] =>
      renderer
        .render(json.schema.value)
        .map: expression =>
          z(
            Typescript.Expression.Call(
              name = "record",
              arguments = List(Typescript.Expression.Symbol("string"), expression)
            )
          )
    case json: Json.Enumeration.Write[A] =>
      z(
        Typescript.Expression.Call(
          name = "enum",
          arguments = List(
            Typescript.Expression.Array(
              elements = json.encode(JsonPrimitiveTypescriptExpressionLiteralEncoder).toList
            )
          )
        )
      ).pure
    case json: Json.Optional.Write[A] =>
      renderer
        .render(json.schema.value)
        .map(expression => z(Typescript.Expression.Call(name = "nullable", arguments = List(expression))))
    case _: Json.Primitive.Boolean.Write[A] =>
      z(Typescript.Expression.Call(name = "boolean", arguments = Nil)).pure
    case _: Json.Primitive.Coerce.Boolean.Write[A] =>
      z(
        Typescript.Expression
          .Member(
            namespace = "coerce",
            property = Typescript.Expression.Call(name = "boolean", arguments = Nil)
          )
      ).pure
    case _: Json.Primitive.Coerce.Number.Write[A] =>
      z(
        Typescript.Expression
          .Member(
            namespace = "coerce",
            property = Typescript.Expression.Call(name = "number", arguments = Nil)
          )
      ).pure
    case _: Json.Primitive.Coerce.Text.Write[A] =>
      z(
        Typescript.Expression
          .Member(
            namespace = "coerce",
            property = Typescript.Expression.Call(name = "string", arguments = Nil)
          )
      ).pure
    case _: Json.Primitive.Number.Write[A] =>
      z(Typescript.Expression.Call(name = "number", arguments = Nil)).pure
    case _: Json.Primitive.Text.Write[A] =>
      z(Typescript.Expression.Call(name = "string", arguments = Nil)).pure
    case json: Json.Record.Write[A] =>
      json.fields
        .map(_.value)
        .toList
        .traverse: field =>
          renderer
            .render(field.schema.value)
            .map: expression =>
              if field.isOptional
              then z(Typescript.Expression.Call(name = "optional", arguments = List(expression)))
              else expression
            .tupleLeft(field.name)
        .map: fields =>
          z(
            Typescript.Expression.Call(
              name = "object",
              arguments = List(Typescript.Expression.Object(fields))
            )
          )
    case json: Json.Tuple.Write[A] =>
      json.schemas
        .map(_.value)
        .traverse(renderer.render)
        .map(expressions => Typescript.Expression.Array(elements = expressions.toList))
        .map(expression => z(Typescript.Expression.Call(name = "tuple", arguments = List(expression))))
    case json: Json.Union.Write[A] =>
      json.branches
        .map(_.value.schema.value)
        .traverse(renderer.render)
        .map(expressions => Typescript.Expression.Array(elements = expressions.toList))
        .map(expression => z(Typescript.Expression.Call(name = "union", arguments = List(expression))))
