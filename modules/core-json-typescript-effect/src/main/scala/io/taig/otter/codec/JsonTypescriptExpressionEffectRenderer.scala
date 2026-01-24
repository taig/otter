package io.taig.otter.codec

import cats.data.Chain
import cats.data.NonEmptyChain
import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.Schema
import io.taig.otter.Typescript
import cats.data.State

final class JsonTypescriptExpressionEffectRenderer(
    renderer: Renderer[[a] =>> Json.Read[a] | Json.Write[a], State[JsonTypescriptEffectContext, Typescript.Expression]]
) extends Renderer[[a] =>> Json.Read[a] | Json.Write[a], State[JsonTypescriptEffectContext, Typescript.Expression]]:
  override def render[A](
      json: Json.Read[A] | Json.Write[A]
  ): State[JsonTypescriptEffectContext, Typescript.Expression] = json match
    case json: Json.Constant.Read[?]    => constant(json.encode(JsonPrimitiveTypescriptExpressionLiteralEncoder)).pure
    case json: Json.Constant.Write[?]   => constant(json.encode(JsonPrimitiveTypescriptExpressionLiteralEncoder)).pure
    case json: Json.Collection.Read[?]  => collection(json.schema.value)
    case json: Json.Collection.Write[?] => collection(json.schema.value)
    case json: Json.Dictionary.Read[?]  => dictionary(json.schema.value)
    case json: Json.Dictionary.Write[?] => dictionary(json.schema.value)
    case json: Json.Enumeration.Read[?] =>
      enumeration(json.encode(JsonPrimitiveTypescriptExpressionLiteralEncoder)).pure
    case json: Json.Enumeration.Write[?] =>
      enumeration(json.encode(JsonPrimitiveTypescriptExpressionLiteralEncoder)).pure
    case json: Json.Optional.Read[?]                                           => optional(json.schema.value)
    case json: Json.Optional.Write[?]                                          => optional(json.schema.value)
    case _: (Json.Primitive.Boolean.Read[?] | Json.Primitive.Boolean.Write[?]) =>
      Schema(Typescript.Expression.Symbol(name = "Boolean")).pure
    case _: (Json.Primitive.Coerce.Boolean.Read[?] | Json.Primitive.Coerce.Boolean.Write[?]) =>
      State: state =>
        val name = "CoerceBoolean"
        val symbol = Typescript.Expression.Symbol(name)
        val tpe = Schema.tpe(Typescript.Type.TypeOf(symbol))
        (state.updated(name, tpe = tpe, expression = Schema.CoerceBoolean), symbol)
    case _: (Json.Primitive.Coerce.Number.Read[?] | Json.Primitive.Coerce.Number.Write[?]) =>
      State: state =>
        val name = "CoerceNumber"
        val symbol = Typescript.Expression.Symbol(name)
        val tpe = Schema.tpe(Typescript.Type.TypeOf(symbol))
        (state.updated(name, tpe, expression = Schema.CoerceNumber), symbol)
    case _: (Json.Primitive.Coerce.Text.Read[?] | Json.Primitive.Coerce.Text.Write[?]) =>
      State: state =>
        val name = "CoerceString"
        val symbol = Typescript.Expression.Symbol(name)
        val tpe = Schema.tpe(Typescript.Type.TypeOf(symbol))
        (state.updated(name, tpe, expression = Schema.CoerceString), symbol)
    case _: (Json.Primitive.Number.Read[?] | Json.Primitive.Number.Write[?]) =>
      Schema(Typescript.Expression.Symbol(name = "Number")).pure
    case _: (Json.Primitive.Text.Read[?] | Json.Primitive.Text.Write[?]) =>
      Schema(Typescript.Expression.Symbol(name = "String")).pure
    case json: Json.Record.Read[?]  => record(json.fields.map(_.value))
    case json: Json.Record.Write[?] => record(json.fields.map(_.value))
    case json: Json.Tuple.Read[?]   => tuple(json.schemas.map(_.value))
    case json: Json.Tuple.Write[?]  => tuple(json.schemas.map(_.value))
    case json: Json.Union.Read[?]   => union(json.branches.map(_.value.schema.value))
    case json: Json.Union.Write[?]  => union(json.branches.map(_.value.schema.value))

  def constant(json: Typescript.Expression): Typescript.Expression =
    Schema(Typescript.Expression.Call(name = "Literal", arguments = List(json)))

  def collection[A](json: Json.Read[A] | Json.Write[A]): State[JsonTypescriptEffectContext, Typescript.Expression] =
    renderer.render(json).map(_.pure[List]).map(Typescript.Expression.Call(name = "Array", _)).map(Schema.apply)

  def dictionary[A](json: Json.Read[A] | Json.Write[A]): State[JsonTypescriptEffectContext, Typescript.Expression] =
    renderer
      .render(json)
      .map(expression => List("key" -> Schema(Typescript.Expression.Symbol("String")), "value" -> expression))
      .map(Typescript.Expression.Object.apply)
      .map(_.pure[List])
      .map(Typescript.Expression.Call(name = "Record", _))
      .map(Schema.apply)

  def enumeration[A](json: NonEmptyChain[Typescript.Expression]): Typescript.Expression =
    Schema(Typescript.Expression.Call(name = "Literal", arguments = json.toList))

  def optional[A](json: Json.Read[A] | Json.Write[A]): State[JsonTypescriptEffectContext, Typescript.Expression] =
    renderer.render(json).map(_.pure[List]).map(Typescript.Expression.Call(name = "NullOr", _)).map(Schema.apply)

  def record[A](
      json: Chain[Json.Field.Read[A] | Json.Field.Write[A]]
  ): State[JsonTypescriptEffectContext, Typescript.Expression] = json.toList
    .traverse:
      case field: Json.Field.Read[?] =>
        renderer
          .render(field.schema.value)
          .map: expression =>
            if field.isOptional
            then Schema(Typescript.Expression.Call(name = "optional", arguments = List(expression)))
            else expression
          .tupleLeft(field.name)
      case field: Json.Field.Write[?] =>
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

  def tuple[A](json: Chain[Json.Read[A] | Json.Write[A]]): State[JsonTypescriptEffectContext, Typescript.Expression] =
    json.toList
      .traverse(renderer.render)
      .map(Typescript.Expression.Call(name = "Tuple", _))
      .map(Schema.apply)

  def union[A](
      json: NonEmptyChain[Json.Read[A] | Json.Write[A]]
  ): State[JsonTypescriptEffectContext, Typescript.Expression] = json.toList
    .traverse(renderer.render)
    .map(Typescript.Expression.Call(name = "Union", _))
    .map(Schema.apply)
