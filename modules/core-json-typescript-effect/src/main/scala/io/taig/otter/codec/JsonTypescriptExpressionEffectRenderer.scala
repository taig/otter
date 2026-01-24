package io.taig.otter.codec

import cats.Applicative
import io.taig.otter.Json
import io.taig.otter.Typescript
import cats.syntax.all.*
import io.taig.otter.Schema

final class JsonTypescriptExpressionEffectRenderer[F[_]: Applicative](
    renderer: Renderer[[a] =>> Json.Read[a] | Json.Write[a], F[Typescript.Expression]]
) extends Renderer[[a] =>> Json.Read[a] | Json.Write[a], F[Typescript.Expression]]:
  override def render[A](json: Json.Read[A] | Json.Write[A]): F[Typescript.Expression] = json match
    case json: Json.Constant.Read[?] =>
      Schema(
        Typescript.Expression.Call(
          name = "Literal",
          arguments = List(json.encode(JsonPrimitiveTypescriptExpressionLiteralEncoder))
        )
      ).pure
    case json: Json.Constant.Write[?] =>
      Schema(
        Typescript.Expression.Call(
          name = "Literal",
          arguments = List(json.encode(JsonPrimitiveTypescriptExpressionLiteralEncoder))
        )
      ).pure
    case json: Json.Collection.Read[?] =>
      renderer
        .render(json.schema.value)
        .map(List(_))
        .map(Typescript.Expression.Call(name = "ReadonlyArray", _))
        .map(Schema.apply)
    case json: Json.Collection.Write[?] =>
      renderer
        .render(json.schema.value)
        .map(List(_))
        .map(Typescript.Expression.Call(name = "ReadonlyArray", _))
        .map(Schema.apply)
    case json: Json.Dictionary.Read[?] =>
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
    case json: Json.Enumeration.Read[?] =>
      Schema(
        Typescript.Expression.Call(
          name = "Literal",
          arguments = json.encode(JsonPrimitiveTypescriptExpressionLiteralEncoder).toList
        )
      ).pure
    case json: Json.Optional.Read[?] =>
      renderer
        .render(json.schema.value)
        .map(_.pure[List])
        .map(Typescript.Expression.Call(name = "NullOr", _))
        .map(Schema.apply)
    case _: Json.Primitive.Boolean.Read[?] =>
      Schema(Typescript.Expression.Symbol(name = "Boolean")).pure
    case _: Json.Primitive.Coerce.Boolean.Read[?] =>
      Typescript.Expression.Symbol(name = "CoerceBoolean").pure
    case _: Json.Primitive.Coerce.Number.Read[?] =>
      Typescript.Expression.Symbol(name = "CoerceNumber").pure
    case _: Json.Primitive.Coerce.Text.Read[?] =>
      Typescript.Expression.Symbol(name = "CoerceString").pure
    case _: Json.Primitive.Number.Read[?] =>
      Schema(Typescript.Expression.Symbol(name = "Number")).pure
    case _: Json.Primitive.Text.Read[?] =>
      Schema(Typescript.Expression.Symbol(name = "String")).pure
    case json: Json.Record.Read[?] =>
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
    case json: Json.Tuple.Read[?] =>
      json.schemas.toList
        .map(_.value)
        .traverse(renderer.render)
        .map(Typescript.Expression.Call(name = "Tuple", _))
        .map(Schema.apply)
    case json: Json.Union.Read[?] =>
      json.branches.toList
        .map(_.value.schema.value)
        .traverse(renderer.render)
        .map(Typescript.Expression.Call(name = "Union", _))
        .map(Schema.apply)
