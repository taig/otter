package io.taig.otter.codec

import cats.Applicative
import io.taig.otter.Json
import io.taig.otter.Typescript
import cats.syntax.all.*
import io.taig.otter.Schema

final class JsonTypescriptExpressionEffectRenderer[F[_]: Applicative](
    renderer: Renderer[Json.Write, F[Typescript.Expression]]
) extends Renderer[Json.Write, F[Typescript.Expression]]:
  override def render[A](json: Json.Write[A]): F[Typescript.Expression] = json match
    case json: Json.Constant.Write[A] =>
      Schema(
        Typescript.Expression.Call(
          name = "Literal",
          arguments = List(json.encode(JsonPrimitiveTypescriptExpressionLiteralEncoder))
        )
      ).pure
    case json: Json.Collection.Write[A] =>
      renderer
        .render(json.schema.value)
        .map(List(_))
        .map(Typescript.Expression.Call(name = "Array", _))
        .map(Schema.apply)
    case json: Json.Dictionary.Write[A] =>
      renderer
        .render(json.schema.value)
        .map: expression =>
          Typescript.Expression.Object(
            fields = List(
              "key" -> Schema(Typescript.Expression.Symbol("String")),
              "value" -> expression
            )
          )
        .map(obj => Schema(Typescript.Expression.Call(name = "Record", arguments = List(obj))))
    case json: Json.Enumeration.Write[A] =>
      Schema(
        Typescript.Expression.Call(
          name = "Literal",
          arguments = json.encode(JsonPrimitiveTypescriptExpressionLiteralEncoder).toList
        )
      ).pure
    case json: Json.Optional.Write[A] =>
      renderer
        .render(json.schema.value)
        .map(_.pure[List])
        .map(Typescript.Expression.Call(name = "NullOr", _))
        .map(Schema.apply)
    case _: Json.Primitive.Boolean.Write[A] =>
      Schema(Typescript.Expression.Symbol(name = "Boolean")).pure
    case _: Json.Primitive.Coerce.Boolean.Write[A] =>
      Typescript.Expression.Symbol(name = "CoerceBoolean").pure
    case _: Json.Primitive.Coerce.Number.Write[A] =>
      Typescript.Expression.Symbol(name = "CoerceNumber").pure
    case _: Json.Primitive.Coerce.Text.Write[A] =>
      Typescript.Expression.Symbol(name = "CoerceString").pure
    case _: Json.Primitive.Number.Write[A] =>
      Schema(Typescript.Expression.Symbol(name = "Number")).pure
    case _: Json.Primitive.Text.Write[A] =>
      Schema(Typescript.Expression.Symbol(name = "String")).pure
    case json: Json.Record.Write[A] =>
      json.fields.toList
        .map(_.value)
        .traverse: field =>
          renderer
            .render(field.schema.value)
            .map: expression =>
              if field.isOptional
              then Schema(Typescript.Expression.Call(name = "optional", arguments = List(expression)))
              else expression
            .tupleLeft(field.name)
        .map: fields =>
          Schema(
            Typescript.Expression.Call(
              name = "Struct",
              arguments = List(Typescript.Expression.Object(fields))
            )
          )
    case json: Json.Tuple.Write[A] =>
      json.schemas.toList
        .map(_.value)
        .traverse(renderer.render)
        .map(Typescript.Expression.Call(name = "Tuple", _))
        .map(Schema.apply)
    case json: Json.Union.Write[A] =>
      json.branches.toList
        .map(_.value.schema.value)
        .traverse(renderer.render)
        .map(Typescript.Expression.Call(name = "Union", _))
        .map(Schema.apply)

final class JsonReadTypescriptExpressionEffectRenderer[F[_]: Applicative](
    renderer: Renderer[Json.Read, F[Typescript.Expression]]
) extends Renderer[Json.Read, F[Typescript.Expression]]:
  override def render[A](json: Json.Read[A]): F[Typescript.Expression] = json match
    case json: Json.Constant.Read[A] =>
      Schema(
        Typescript.Expression.Call(
          name = "Literal",
          arguments = List(json.encode(JsonPrimitiveTypescriptExpressionLiteralEncoder))
        )
      ).pure
    case json: Json.Collection.Read[A] =>
      renderer
        .render(json.schema.value)
        .map(List(_))
        .map(Typescript.Expression.Call(name = "Array", _))
        .map(Schema.apply)
    case json: Json.Dictionary.Read[A] =>
      renderer
        .render(json.schema.value)
        .map: expression =>
          Typescript.Expression.Object(
            fields = List(
              "key" -> Schema(Typescript.Expression.Symbol("String")),
              "value" -> expression
            )
          )
        .map(obj => Schema(Typescript.Expression.Call(name = "Record", arguments = List(obj))))
